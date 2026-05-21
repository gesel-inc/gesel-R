#include "Rcpp.h"
#include "gesel/gesel.hpp"

//[[Rcpp::export(rng=false)]]
SEXP validate_database_files(std::string db_prefix, int num_genes) {
    gesel::validate_database(db_prefix, num_genes);
    return R_NilValue;
}

//[[Rcpp::export(rng=false)]]
int validate_gene_files(std::string gene_prefix, Rcpp::Nullable<Rcpp::CharacterVector> types) {
    if (types.isNull()) {
        return gesel::validate_genes(gene_prefix);
    } 

    Rcpp::CharacterVector realized(types);
    std::vector<std::string> name_types;
    name_types.reserve(realized.size());
    for (auto t : realized) {
        name_types.push_back(Rcpp::as<std::string>(t));
    }
    return gesel::validate_genes(gene_prefix, name_types);
}
