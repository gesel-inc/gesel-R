#include "Rcpp.h"

#include <cstring>
#include <string>
#include <stdexcept>
#include <cstddef>

//[[Rcpp::export(rng=false)]]
Rcpp::List parse_multipart_ranges(Rcpp::RawVector body, std::string boundary) {
    const char* body_ptr = reinterpret_cast<const char*>(static_cast<const unsigned char*>(body.begin()));
    const std::size_t body_len = body.size();
    std::size_t position = 0;

    const std::string augmented_boundary = "--" + boundary;
    const std::size_t augmented_size = augmented_boundary.size();

    // Searching for the first boundary.
    while (1) {
        if (body_len - position < augmented_size) {
            throw std::runtime_error("premature termination of multipart body");
        }
        if (std::strncmp(augmented_boundary.c_str(), body_ptr + position, augmented_size) == 0) {
            position += augmented_size;
            break;
        }

        for (; position < body_len; ++position) {
            const char val = body_ptr[position];
            if (val == '\r') {
                auto tmp = position + 1;
                if (body_len - tmp >= 1 && body_ptr[tmp] == '\n') {
                    position = tmp + 1;
                    break;
                }
            }
        }
    }

    std::vector<Rcpp::RawVector> output;
    std::vector<std::pair<std::string, std::string> > headers;

    // Loop over all parts, extracting out part-specific headers where relevant.
    while (1) {
        if (body_len - position < 2) {
            throw std::runtime_error("premature termination of multipart body");
        }
        if (body_ptr[position] == '-' && body_ptr[position + 1] == '-') {
            // Just ignore what's left, if anything, after the ending boundary.
            break;
        }

        for (; position < body_len && body_ptr[position] == ' '; ++position) {} // Chomping whitespace before the CRLF.
        if (body_ptr[position] != '\r' || body_ptr[position + 1] != '\n') {
            throw std::runtime_error("boundary marker should be followed by a CRLF at position " + std::to_string(position));
        }
        position += 2;

        // Parsing the current headers.
        headers.clear();
        while (1) { 
            if (body_len - position < 1) {
                throw std::runtime_error("premature termination of multipart body");
            }

            if (body_ptr[position] == '\r') {
                ++position;
                if (body_len - position < 1) {
                    throw std::runtime_error("premature termination of multipart body");
                }
                if (body_ptr[position] != '\n') {
                    throw std::runtime_error("end of header lines should be marked by an empty line at position " + std::to_string(position));
                }
                ++position;
                break;
            }

            std::string header_name;
            for (; position < body_len; ++position) {
                const char val = body_ptr[position];
                if (val == ':') {
                    ++position;
                    break;
                }
                header_name += val;
            }

            if (body_len - position < 1) {
                throw std::runtime_error("premature termination of multipart body");
            }
            if (body_ptr[position] != ' ') {
                throw std::runtime_error("each header name should be followed by a space at position " + std::to_string(position));
            }
            ++position;

            std::string header_val;
            for (; position < body_len; ++position) {
                const char val = body_ptr[position];
                if (val == '\r') {
                    ++position;
                    break;
                }
                header_val += val;
            }

            if (body_len - position < 1) {
                throw std::runtime_error("premature termination of multipart body");
            }
            if (body_ptr[position] != '\n') {
                throw std::runtime_error("header lines should be followed by a CRLF at position " + std::to_string(position));
            }
            ++position;

            headers.emplace_back(std::move(header_name), std::move(header_val));
        }

        // Searching for the next boundary (including its CRLF) to find the range of bytes comprising the current part.
        const auto start = position;

        while (1) {
            if (position == body_len) {
                throw std::runtime_error("premature termination of multipart body");
            }

            if (body_ptr[position] == '\r') {
                auto tmp = position + 1;
                if (body_len - tmp >= 1 && body_ptr[tmp] == '\n') {
                    ++tmp;
                    if (body_len - tmp >= augmented_size && std::strncmp(augmented_boundary.data(), body_ptr + tmp, augmented_size) == 0) {
                        tmp += augmented_size;
                        position = tmp;
                        break;
                    }
                }
            }

            ++position;
        }

        output.emplace_back(body.begin() + start, body.begin() + position - augmented_size - 2); // don't include the boundary + CRLF in the part's contents.
        for (const auto& header : headers) {
            output.back().attr(header.first) = header.second;
        }
    }

    return Rcpp::List(output.begin(), output.end());
}
