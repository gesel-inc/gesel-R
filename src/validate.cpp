#include "Rcpp.h"

#include "gesel/gesel.hpp"
#include "sanisizer/sanisizer.hpp"

//[[Rcpp::export(rng=false)]]
SEXP validate_database_files(std::string db_prefix, int num_genes) {
    if (num_genes < 0) {
        // If it's a double that doesn't fit in an integer, it'll be cast to NA and picked up here.
        throw std::runtime_error("expected 'num_genes' to be non-zero");
    }
    gesel::validate_database(db_prefix, sanisizer::Cast(num_genes));
    return R_NilValue;
}

//[[Rcpp::export(rng=false)]]
int validate_gene_files(std::string gene_prefix, Rcpp::Nullable<Rcpp::CharacterVector> types) {
    if (types.isNull()) {
        const auto num_genes = gesel::validate_genes(gene_prefix);
        return sanisizer::cast<int>(num_genes);
    } 

    Rcpp::CharacterVector realized(types);
    std::vector<std::string> name_types;
    name_types.reserve(realized.size());
    for (auto t : realized) {
        name_types.push_back(Rcpp::as<std::string>(t));
    }

    const auto num_genes = gesel::validate_genes(gene_prefix, name_types);
    return sanisizer::cast<int>(num_genes);
}
