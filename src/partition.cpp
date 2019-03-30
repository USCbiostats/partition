#include <RcppArmadillo.h>
#include "utils.h"
#include "minr2.h"
#include "corr.h"
#include "icc.h"
#include "pca.h"

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
NumericMatrix update_dist(NumericMatrix reduced_dist, CharacterVector cluster_nm,  
                          CharacterVector clust_var_nms, NumericMatrix reduced_data, 
                          std::string dist_type) {
  IntegerVector cluster_index, distance_index;
  NumericMatrix cluster_var, other_vars, cluster_dist;
  cluster_index = which(is_element(colnames(reduced_data), cluster_nm));
                           
	cluster_var = subset_c(reduced_data, cluster_index);
	other_vars = drop(reduced_data, cluster_index);

	//nms = colnames(dat.r)[colnames(dat.r) %nin% cluster.nm]
	//dat.r.tmp = as.data.frame( dat.r[, nms ] )
	//names( dat.r.tmp ) = nms
	// compute distances between myvar and other variables
	//if( !is.na(dim(dat.r.tmp)[1]) ){
		if (dist_type == "s") {
			cluster_dist = spearman_distance(other_vars, cluster_var);
		} else {
			cluster_dist = pearson_distance(other_vars, cluster_var);
		}
		//colnames(cluster_dist) = colnames(other_vars);
	//}

	// remove cluster component variables from distance matrix
  distance_index = which(is_element(colnames(reduced_dist), clust_var_nms));
	reduced_dist = drop_dist(reduced_dist, distance_index);

	//if( !is.null(dim(dist.r)[1])){
	  CharacterVector reduced_colnames;
	  reduced_colnames = colnames(reduced_dist);
	  reduced_colnames.push_back(cluster_nm[0]);
	 
	  NumericMatrix out_dist, bound_dist;
    bound_dist = cbind(reduced_dist, cluster_dist);
    

//     CharacterVector reduced_rownames;
// 		reduced_rownames = rownames(reduced_dist);
// 	  reduced_rownames.push_back(cluster_nm[0]);
		out_dist = bind_empty_row(bound_dist);
		colnames(out_dist) = reduced_colnames;
		rownames(out_dist) = reduced_colnames;

	//}
	
	return out_dist;
} 


// // [[Rcpp::export]]
// List assign_clusters(IntegerVector index, NumericMatrix reduced_dist, 
//                      NumericMatrix reduced_data_r, NumericMatrix data_r, 
//                      double pct_var, DataFrame clusters_r, int cluster_ind, 
//                      std::string method, std::string dist_type, std::string new_var){
// 	bool success = false;
//   NumericMatrix data, reduced_data;
//   DataFrame clusters = clone(clusters_r);
//   data = clone(data_r);
//   reduced_data = clone(reduced_data_r);
// 	CharacterVector clust_colnames, original_vars, row_names, clust_var_nms, clust_var_nms_raw;
//   IntegerVector var_ind;
//   clust_colnames = colnames(reduced_data);
//   original_vars = clusters.attr("row.names");
//   row_names = clone(original_vars);
//   clust_var_nms = clust_colnames[index - 1];
//   
// 	clust_var_nms_raw = original_vars[is_element(clusters["cluster"], clust_var_nms)];
// 	std::string cluster_nm = new_var.append(std::to_string(cluster_ind));
// 	
// 	// 2. compute summary and test against pct.var
// 	
// 	var_ind = which(is_element(original_vars, clust_var_nms_raw));
// 	List tmp_svec; 
// 	NumericMatrix x = subset_c(data, var_ind);
// 	
// 	if (method == "ICC") {
// 	  tmp_svec = ICC_c(x);
// 	} else if (method == "MI") {
// 	  Function MI("MI");
// 	  tmp_svec = MI(x);
// 	} else if (method == "minR2") {
// 	  tmp_svec = minR2_c(x);
// 	} else {
// 	  tmp_svec = pca_c(as<arma::mat>(x));
// 	}
//                     
// 
// 	// 3. update, dist.r, dat.r, clusters, cluster.new.names
// 	double clstr_pct_var;
// 	NumericVector clstr_data;
// 	clstr_pct_var = tmp_svec[0];
// 	clstr_data = tmp_svec[1];
// 	
//   CharacterVector cluster_cluster;
//   NumericVector cluster_pct_var;
//   LogicalVector cluster_index;
//   DataFrame mappings;
//   NumericMatrix out_data, out_dist;
//   
// 	if( clstr_pct_var >= pct_var ){
// 			success = true;
// 
// 	    // update cluster mappings
// 
// 	    cluster_cluster = clusters["cluster"];
// 	    cluster_pct_var = clusters["pct.var"];
// 	    cluster_index = is_element(cluster_cluster, clust_var_nms_raw);
// 
// 	    for (int i = 0; i < cluster_index.size(); i++) {
// 	      if (cluster_index[i]) {
// 	        cluster_cluster[i] = cluster_nm;
// 	        cluster_pct_var[i] = clstr_pct_var;
//   	      }
//   	    }
// 
// 			mappings = DataFrame::create(_["cluster"]= cluster_cluster,
//                      _["pct.var"] = cluster_pct_var);
//       mappings.attr("row.names") = row_names;
// 			// increment to form unique name for next formed cluster
// 			cluster_ind++;
// 
// 			// add to reduced data and update names
// 			out_data = cbind(reduced_data, clstr_data); // add summary variable to dat.r
// 			CharacterVector new_colnames;
// 			new_colnames = colnames(reduced_data);
// 			new_colnames.push_back(cluster_nm);
// 			colnames(out_data) = new_colnames;
// 			out_data = drop(out_data, index);
// 			//  aa = !is.null(dim(dat.r)[1])
// 			//if( aa )
// 			
// 		out_dist = update_dist(reduced_dist, cluster_nm, clust_var_nms, out_data, dist_type);
// 	} else {
// 	  // set distance to NA to avoid testing again
// 		reduced_dist(index[0], index[1]) = NumericMatrix::get_na();
// 	  out_data = reduced_data;
// 	  out_dist = reduced_dist;
// 	}
// 	
// 	return List::create(mappings, cluster_ind, out_dist, out_data, success);
// }


// [[Rcpp::export]]
List assign_clusters(IntegerVector index_r, NumericMatrix reduced_dist_r, 
                     NumericMatrix reduced_data_r, NumericMatrix data_r, 
                     double pct_var, DataFrame clusters_r, int cluster_ind, 
                     std::string method, std::string dist_type, std::string new_var){
	bool success = false;
  NumericMatrix data, reduced_data, reduced_dist;
  DataFrame clusters = clone(clusters_r);
  data = clone(data_r);
  reduced_data = clone(reduced_data_r);
  reduced_dist = clone(reduced_dist_r);
	CharacterVector clust_colnames, original_vars, row_names, clust_var_nms, clust_var_nms_raw;
  IntegerVector var_ind, index;
  index = clone(index_r) - 1;
  clust_colnames = colnames(reduced_data);
  original_vars = clusters.attr("row.names");
  row_names = clone(original_vars);
  clust_var_nms = clust_colnames[index];
  
	clust_var_nms_raw = row_names[is_element(clusters["cluster"], clust_var_nms)];
	
	std::string cluster_nm = new_var.append(std::to_string(cluster_ind));
	
	// 2. compute summary and test against pct.var
	
	var_ind = which(is_element(original_vars, clust_var_nms_raw));
	List tmp_svec; 
	NumericMatrix x = subset_c(data, var_ind);
	
	if (method == "ICC") {
	  tmp_svec = ICC_c(x);
	} else if (method == "MI") {
	  Environment pkg = Environment::namespace_env("partition");
    Function MI = pkg["MI"];
	  tmp_svec = MI(x);
	} else if (method == "minR2") {
	  tmp_svec = minR2_c(x);
	} else {
	  tmp_svec = pca_c(as<arma::mat>(x));
	}
                    

	// 3. update, dist.r, dat.r, clusters, cluster.new.names
	double clstr_pct_var;
	NumericVector clstr_data;
	clstr_pct_var = tmp_svec[0];
	clstr_data = tmp_svec[1];
	
  CharacterVector cluster_cluster;
  NumericVector cluster_pct_var;
  LogicalVector cluster_index;
  DataFrame mappings;
  NumericMatrix out_data, out_dist;
  
	if( clstr_pct_var >= pct_var ){
			success = true;

	    // update cluster mappings

	    cluster_cluster = clusters["cluster"];
	    cluster_pct_var = clusters["pct.var"];
	    cluster_index = is_element(row_names, clust_var_nms_raw);
     
	    for (int i = 0; i < cluster_index.size(); i++) {
	      if (cluster_index[i]) {
	        cluster_cluster[i] = cluster_nm;
	        cluster_pct_var[i] = clstr_pct_var;
  	      }
  	    }

			mappings = DataFrame::create(_["cluster"]= cluster_cluster,
                     _["pct.var"] = cluster_pct_var);
      mappings.attr("row.names") = row_names;
			// increment to form unique name for next formed cluster
			cluster_ind++;

			// add to reduced data and update names
			// CharacterVector row_incides;
			// row_incides = rownames(reduced_data);
			out_data = cbind(reduced_data, clstr_data); // add summary variable to dat.r
			CharacterVector new_colnames;
			new_colnames = colnames(reduced_data);
			new_colnames.push_back(cluster_nm);
			colnames(out_data) = new_colnames;
			out_data = drop(out_data, index);
			
			//rownames(out_data) = row_incides;
			//  aa = !is.null(dim(dat.r)[1])
			//if( aa )
			
		out_dist = update_dist(reduced_dist, cluster_nm, clust_var_nms, out_data, dist_type);
	} else {
	  // set distance to NA to avoid testing again
		reduced_dist(index[0], index[1]) = NumericMatrix::get_na();
	  out_data = reduced_data;
	  mappings = clusters;
	  out_dist = reduced_dist;
	}
	
	return List::create(mappings, cluster_ind, out_dist, out_data, success);
}