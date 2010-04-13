/**
 * R/C Interface code for HDF5 file format. 
 *
 * 
 *
 * 10/04/2010: refactored interface to hide most of the type 
 *             determination in C. JHB
 */
#include <hdf5.h>
#include <hdf5_hl.h>
#include <Rinternals.h>    
#include <R.h>

#define DEBUG 0
#define HID(argname) (((h5_holder*) R_ExternalPtrAddr(argname))->id)
#define NM(argname) (CHAR(STRING_ELT(argname, 0)))

typedef struct h5_holder {
    hid_t id;
} h5_holder;

SEXP h5R_open(SEXP filename) {
    const char* f_name = NM(filename);
    h5_holder* holder = (h5_holder*) Calloc(1, h5_holder);
    holder->id = H5Fopen(f_name, H5F_ACC_RDONLY, H5P_DEFAULT);
    return R_MakeExternalPtr(holder, install("hd5_handle"), R_NilValue);
}

SEXP h5R_close(SEXP h5_file) {
    H5Fclose(HID(h5_file));
    Free(h5_file);
    return R_NilValue;
}

SEXP h5R_get_dim(SEXP h5_file, SEXP dataset) {
    SEXP ans;
    int rank = -1; 
    int i;
      
    /* determine the number of dimensions. */
    H5LTget_dataset_ndims(HID(h5_file), NM(dataset), &rank);
    hsize_t* dims = Calloc((size_t) rank, hsize_t);
    herr_t status = H5LTget_dataset_info(HID(h5_file), NM(dataset), dims, NULL, NULL); 

    /* vectors with only length are represented as column vectors. */
    if (rank == 1) {
	PROTECT(ans = allocVector(INTSXP, 2));
	INTEGER(ans)[0] = dims[0];
	INTEGER(ans)[1] = 1;
    }
    else {
	PROTECT(ans = allocVector(INTSXP, rank));
	for (i = 0; i < rank; i++) 
	    INTEGER(ans)[i] = dims[i];
    }
    UNPROTECT(1);
    Free(dims);
    
    return ans;
}

SEXP h5R_get_group(SEXP h5_obj, SEXP group_name) {
    h5_holder* holder = (h5_holder*) Calloc(1, h5_holder);
    holder->id = H5Gopen(HID(h5_obj), NM(group_name), H5P_DEFAULT);
    return R_MakeExternalPtr(holder, install("hd5_handle"), R_NilValue);
}

SEXP h5R_get_dataset(SEXP h5_obj, SEXP dataset_name) {
    h5_holder* holder = (h5_holder*) Calloc(1, h5_holder);
    holder->id = H5Dopen(HID(h5_obj), NM(dataset_name), H5P_DEFAULT);
    return R_MakeExternalPtr(holder, install("hd5_handle"), R_NilValue);
}

SEXP h5R_get_type(SEXP h5_obj, SEXP dataset_name) {
    SEXP dtype_id;

    hid_t id = H5Dopen(HID(h5_obj), NM(dataset_name), H5P_DEFAULT);
    hid_t cls_id = H5Dget_type(id);
    H5T_class_t cls = H5Tget_class(cls_id);

    PROTECT(dtype_id = allocVector(INTSXP, 1));
    switch (cls) {
    case H5T_INTEGER: 
	INTEGER(dtype_id)[0] = 1;
	break;
    case H5T_FLOAT:
	INTEGER(dtype_id)[0] = 2;
	break;
    case H5T_STRING:
	INTEGER(dtype_id)[0] = 3;
	break;
    default:
	INTEGER(dtype_id)[0] = -1;
    }

    H5Tclose(cls_id);
    H5Dclose(id);
    
    UNPROTECT(1);
    return(dtype_id);
}

SEXP h5R_get_dataset_str(SEXP h5_dataset) {
    int j,i;
    SEXP res;
    hsize_t dims[1] = {0};

    hid_t dset = HID(h5_dataset);

    /*
     * Get the datatype.
     */
    hid_t filetype = H5Dget_type (dset);

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    hid_t space = H5Dget_space (dset);
    int ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    char** rdata = (char **) malloc(dims[0]*sizeof(char*));

    /*
     * Create the memory datatype.
     */
    hid_t memtype = H5Tcopy (H5T_C_S1);
    herr_t status = H5Tset_size (memtype, H5T_VARIABLE);
    
    /*
     * Read the data.
     */
    status = H5Dread(dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

    /*
     * Determine the size of the vector to allocate.
     */
    /* j = 0; */
    /* for (i=0; i<dims[0]; i++) */
    /* 	if (rdata[i]) j++; */
    j = dims[0];
    
    PROTECT(res = allocVector(STRSXP, j));
    for (i = 0; i < j; i++) {
	if (rdata[i])
	    SET_STRING_ELT(res, i, mkChar(rdata[i]));
    }
    UNPROTECT(1);

    /*
     * Close and release resources.  Note that H5Dvlen_reclaim works
     * for variable-length strings as well as variable-length arrays.
     * Also note that we must still free the array of pointers stored
     * in rdata, as H5Tvlen_reclaim only frees the data these point to.
     */
    status = H5Dvlen_reclaim (memtype, space, H5P_DEFAULT, rdata);
    free(rdata);

    status = H5Sclose (space);
    status = H5Tclose (filetype);
    status = H5Tclose (memtype);

    return res;
}

SEXP h5R_read_dataset(SEXP h5_dataset, SEXP _rSize, SEXP _rType) {
    int rType = INTEGER(_rType)[0];
    int rSize = INTEGER(_rSize)[0];
    int up = 0;

    SEXP dta;
    hid_t memtype;

    void* buf; 

    switch (rType) {
    case 1 : 
	up++;
	PROTECT(dta = allocVector(INTSXP, rSize));
	memtype = H5T_NATIVE_INT;
	buf = INTEGER(dta);
	break;
    case 2 :
	up++;
	PROTECT(dta = allocVector(REALSXP, rSize));
	memtype = H5T_NATIVE_DOUBLE;
	buf = REAL(dta);
	break;
    case 3 :
	/** There is a possibility to refactor this as well,
	    however, it doesn't seem to warrant that at the 
	    moment. **/
	return h5R_get_dataset_str(h5_dataset);
    }
    
    H5Dread(HID(h5_dataset), memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);

    UNPROTECT(up);
    return(dta);
    
}

  
SEXP h5R_get_attribute(SEXP h5_obj, SEXP attr_name) {
    hid_t attr = H5Aopen(HID(h5_obj), NM(attr_name), H5P_DEFAULT);
    hid_t memtype = H5Aget_type(attr);
    hid_t space = H5Aget_space(attr);

    /** this is assuming 1-d attributes. **/
    hsize_t dims[1] = {1}; 
    int ndims = H5Sget_simple_extent_dims(space, dims, NULL);

    SEXP dta; 
    void* buf;
    int up = 0;

    switch (H5Tget_class(memtype)) {
    case H5T_INTEGER:
	PROTECT(dta = allocVector(INTSXP, dims[0]));
	buf = INTEGER(dta);
	up++;
	break;
    case H5T_FLOAT:
	PROTECT(dta = allocVector(REALSXP, dims[0]));
	buf = REAL(dta);
	up++;
	break;
    case H5T_STRING:
	return R_NilValue;
    default:
	return R_NilValue;
    }

    H5Aread(attr, memtype, buf);
    UNPROTECT(up);

    H5Aclose(attr);
    H5Tclose(memtype);
    H5Sclose(space);

    return dta;
}



/**
   below is code that I was using before refactoring. 
**/


/* size_t _get_dataset_size(hid_t file, const char* ds_name) { */
/*     int rank    =-1; */
/*     size_t size = 1; */
/*     int i;  */
   
/*     H5LTget_dataset_ndims(file, ds_name, &rank); */
/*     hsize_t* dims = Calloc((size_t) rank, hsize_t); */
/*     herr_t status = H5LTget_dataset_info(file, ds_name, dims, NULL, NULL);  */

/*     for (i = 0; i < rank; i++)  */
/* 	size *= dims[i]; */

/*     Free(dims); */
/*     return size; */
/* } */

/* SEXP h5R_get_dataset_int(SEXP h5_file, SEXP dataset) { */
/*     SEXP data; herr_t status; */
/*     size_t v = _get_dataset_size(HID(h5_file), NM(dataset)); */
/*     PROTECT(data = allocVector(INTSXP, v)); */
/*     status = H5LTread_dataset_int(HID(h5_file), NM(dataset), INTEGER(data)); */
/*     UNPROTECT(1); */
/*     return data; */
/* } */

/* SEXP h5R_get_dataset_float(SEXP h5_file, SEXP dataset) { */
/*     SEXP data; herr_t status; */
/*     size_t v = _get_dataset_size(HID(h5_file), NM(dataset)); */
/*     PROTECT(data = allocVector(REALSXP, v)); */
/*     status = H5LTread_dataset_double(HID(h5_file), NM(dataset), REAL(data)); */
/*     UNPROTECT(1); */
/*     return data; */
/* } */

/* SEXP h5R_get_attribute_int(SEXP h5_obj, SEXP ds_name, SEXP attr_name)  */
/* { */
/*     int rank = -1; */
/*     size_t size = 1; */
/*     SEXP data; */
/*     int v = 1; */
/*     int i; */

/*     Rprintf("dataset: %s, attribute: %s\n", NM(ds_name), NM(attr_name)); */
/*     H5LTget_attribute_ndims(HID(h5_obj), NM(ds_name), NM(attr_name), &rank); */
/*     Rprintf("rank %d\n", rank); */

/*     if (rank > 0) { */
/* 	hsize_t* dims = Calloc((size_t) rank, hsize_t); */
/* 	H5LTget_attribute_info(HID(h5_obj), NM(ds_name),  */
/* 			       NM(attr_name), dims, NULL, NULL);  */
/* 	for (i = 0; i < rank; i++) */
/* 	    v *= dims[i]; */
/* 	PROTECT(data = allocVector(INTSXP, v)); */
/*     } else { */
/* 	PROTECT(data = allocVector(INTSXP, 1)); */
/*     } */
/*     Rprintf("allocated data.\n"); */

/*     H5LTget_attribute_int(HID(h5_obj), NM(ds_name), NM(attr_name), INTEGER(data)); */
/*     UNPROTECT(1); */
/*     return data; */
/* } */
