#include "Rcpp.h"

#include <type_traits>

#include "sanisizer/sanisizer.hpp"

template<typename Input_>
using I = std::remove_cv_t<std::remove_reference_t<Input_> >;

//[[Rcpp::export(rng=false)]]
Rcpp::List extract_sorted_byte_ranges(
    Rcpp::List retrieved_contents,
    Rcpp::IntegerVector retrieved_starts,
    Rcpp::IntegerVector retrieved_ends, 
    Rcpp::IntegerVector requested_starts,
    Rcpp::IntegerVector requested_ends
) {
    const auto nretrieved = retrieved_contents.size();
    std::vector<Rcpp::RawVector> payloads;
    payloads.reserve(nretrieved);
    for (I<decltype(nretrieved)> i = 0; i < nretrieved; ++i) {
        payloads.emplace_back(retrieved_contents[i]);
    }

    const auto nrequested = requested_starts.size();
    auto output = sanisizer::create<Rcpp::List>(nrequested);
    I<decltype(nretrieved)> ret_count = 0;
    for (I<decltype(nrequested)> r = 0; r < nrequested; ++r) {
        const auto cur_req_start = requested_starts[r];
        const auto cur_req_end = requested_ends[r];

        // Recall that both sets of retrieved/requested ranges are sorted by increasing start position.
        // Our approach is to search for the first retrieved range that encloses our requested range.
        // We only need to pick up from the previous retrieved range because it's impossible for a later requested range to only overlap a previous retrieved range.
        //
        // Specifically, let X_i and Y_i be the start and end of retrieved range i, while P_j and Q_j are the start and end of the requested range j.
        // Let i be the first retrieved range that encloses j, i.e., X_i <= P_j and Y_i >= Q_j but Y_h < Q_j for all h < i.
        // (We know that X_h <= X_i due to sorting, so for h to not enclose j, the end position of j must exceed that of h.)
        // Now, consider a requested range k > j where k is not enclosed by i, i.e., Y_i < Q_k. 
        // (Again, we know that P_j <= P_k due to sorting, so for i to not enclose k, the end position of k must exceed that of i.)
        // This gives us Y_h < Q_j <= Y_i < Q_k, such that it is impossible for any h to enclose k.
        //
        // Note that this logic works with any degree of nesting.
        // If a nested retrieved range is sorted after its parent, it is just ignored, as only the parent will be used.
        // If it's sorted before the parent due to a tied start, then it will be used until its end is exceeded and the algorithm moves onto the parent.
        while (ret_count != nretrieved && !(retrieved_starts[ret_count] <= cur_req_start && cur_req_end <= retrieved_ends[ret_count])) {
            ++ret_count;
        }
        if (ret_count == nretrieved) {
            throw std::runtime_error("failed to find an enclosing byte range for a requested range");
        }

        const auto& target = payloads[ret_count];
        const auto offset = retrieved_starts[ret_count];
        output[r] = Rcpp::RawVector(target.begin() + (cur_req_start - offset), target.begin() + (cur_req_end - offset));
    }

    return output;
}
