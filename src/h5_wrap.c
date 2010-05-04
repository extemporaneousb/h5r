/**
 * R/C Interface code for HDF5 file format. 
 *
 */
#include <hdf5.h>
#include <Rinternals.h>    
#include <R.h>

#define DEBUG 0
#define HID(argname) (((h5_holder*) R_ExternalPtrAddr(argname))->id)
#define NM(argname) (CHAR(STRING_ELT(argname, 0)))

typedef struct h5_holder {
    int is_file;
    hid_t id;
} h5_holder;

void h5R_finalizer(SEXP h5_obj) {
    h5_holder* h = (h5_holder*) R_ExternalPtrAddr(h5_obj);

    if (! h) return;
    if (h->is_file)
	H5Fclose(HID(h5_obj));
 
    Free(h);
}

SEXP _h5R_make_holder (hid_t id, int is_file) {
    h5_holder* holder = (h5_holder*) Calloc(1, h5_holder);
    holder->id = id;
    holder->is_file = is_file;
    
    SEXP e_ptr = R_MakeExternalPtr(holder, install("hd5_handle"), R_NilValue); 
    R_RegisterCFinalizerEx(e_ptr, h5R_finalizer, TRUE);

    return e_ptr;
}

SEXP h5R_open(SEXP filename) {
    return _h5R_make_holder(H5Fopen(NM(filename), H5F_ACC_RDONLY, H5P_DEFAULT), 1);
}

SEXP h5R_get_group(SEXP h5_obj, SEXP group_name) {
    return _h5R_make_holder(H5Gopen(HID(h5_obj), NM(group_name), H5P_DEFAULT), 0);
}

SEXP h5R_get_dataset(SEXP h5_obj, SEXP dataset_name) {
    return _h5R_make_holder(H5Dopen(HID(h5_obj), NM(dataset_name), H5P_DEFAULT), 0);
}

SEXP h5R_get_attr(SEXP h5_obj, SEXP attr_name) {
    return _h5R_make_holder(H5Aopen(HID(h5_obj), NM(attr_name), H5P_DEFAULT), 0);
}

SEXP h5R_get_type(SEXP h5_obj) {
    SEXP dtype = R_NilValue;
    hid_t cls_id = -1;

    switch (H5Iget_type(HID(h5_obj))) {
    case H5I_DATASET:
	cls_id = H5Dget_type(HID(h5_obj));
	break;
    case H5I_ATTR:
	cls_id = H5Aget_type(HID(h5_obj));
	break;
    default:
	error("Unkown object in %s.\n", __func__);
    }

    PROTECT(dtype = ScalarInteger(H5Tget_class(cls_id)));
    H5Tclose(cls_id);
    UNPROTECT(1);

    return(dtype);
}

hid_t _h5R_get_space(SEXP h5_obj) {
    hid_t space = -1;

    switch (H5Iget_type(HID(h5_obj))) {
    case H5I_DATASET:
	space = H5Dget_space(HID(h5_obj));
	break;
    case H5I_ATTR:
	space = H5Aget_space(HID(h5_obj));
	break;
    default:
	error("Unknown object in %s.\n", __func__);
    }
    return space;
}

int _h5R_get_ndims(SEXP h5_obj) {
    hid_t space = _h5R_get_space(h5_obj);
    int ndims = H5Sget_simple_extent_ndims(space);
    return ((ndims < 0) ? 1 : ndims);
}

int _h5R_get_nelts(SEXP h5_obj) {
    int v = 1; int i;
    int ndims = _h5R_get_ndims(h5_obj);
    hid_t space = _h5R_get_space(h5_obj);
    
    hsize_t* dims = (hsize_t* ) Calloc(ndims, hsize_t*);
    H5Sget_simple_extent_dims(space, dims, NULL);
    
    for (i = 0; i < ndims; i++)
	v *= dims[i];
    
    Free(dims);
    H5Sclose(space);

    return(v);
}

SEXP h5R_get_dims(SEXP h5_obj) {
    int i; SEXP res; 
    int ndims = _h5R_get_ndims(h5_obj);
    hid_t space = _h5R_get_space(h5_obj);

    hsize_t* dims = (hsize_t* ) Calloc(ndims, hsize_t*);
    H5Sget_simple_extent_dims(space, dims, NULL);
    
    PROTECT(res = allocVector(INTSXP, ndims)); 
    for (i = 0; i < ndims; i++)
	INTEGER(res)[i] = dims[i];
    UNPROTECT(1);
    
    Free(dims);
    H5Sclose(space);
    
    return res;
}

SEXP _h5R_read_vlen_str(SEXP h5_obj) {
    int i;

    SEXP res  = R_NilValue;
    int nelts = _h5R_get_nelts(h5_obj);

    char** rdata  = (char **) Calloc(nelts, char*);
    hid_t memtype = H5Tcopy (H5T_C_S1);
    H5Tset_size (memtype, H5T_VARIABLE);
    
    switch (H5Iget_type(HID(h5_obj))) {
    case H5I_DATASET:
	H5Dread(HID(h5_obj), memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);
	break;
    case H5I_ATTR:
	H5Aread(HID(h5_obj), memtype, rdata);
	break;
    default:
	error("Unsupported class in %s.\n", __func__);
    }
    
    PROTECT(res = allocVector(STRSXP, nelts));
    for (i = 0; i < nelts; i++)
	if (rdata[i])
     	    SET_STRING_ELT(res, i, mkChar(rdata[i])); 
    UNPROTECT(1); 

    H5Dvlen_reclaim (memtype, _h5R_get_space(h5_obj), H5P_DEFAULT, rdata);
    Free(rdata);
    H5Tclose(memtype);

    return res;
}

SEXP h5R_read_dataset(SEXP h5_dataset) {
    SEXP dta = R_NilValue;
    hid_t memtype = -1;
    void* buf = NULL; 

    switch (INTEGER(h5R_get_type(h5_dataset))[0]) {
    case H5T_INTEGER: 
	PROTECT(dta = allocVector(INTSXP, _h5R_get_nelts(h5_dataset)));
	memtype = H5T_NATIVE_INT;
	buf = INTEGER(dta);
	break;
    case H5T_FLOAT:
	PROTECT(dta = allocVector(REALSXP, _h5R_get_nelts(h5_dataset)));
	memtype = H5T_NATIVE_DOUBLE;
	buf = REAL(dta);
	break;
    case H5T_STRING:
	return _h5R_read_vlen_str(h5_dataset);
    default:
	error("Unsupported class in %s.\n", __func__);
    }

    H5Dread(HID(h5_dataset), memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf); 
    UNPROTECT(1);

    return(dta);
}

SEXP h5R_read_slab(SEXP h5_dataset, SEXP _offsets, SEXP _counts) {
    SEXP dta = R_NilValue;
    hid_t space = -1, memspace = -1, memtype = -1;
    void* buf = NULL; 
    int i; 

    int* offsets  = INTEGER(_offsets);
    int* counts   = INTEGER(_counts);

    /** I'm surprised I have to do this, but it seems to be necessary. **/
    hsize_t* _h_offsets = (hsize_t*) Calloc(length(_counts), hsize_t);
    hsize_t* _h_counts  = (hsize_t*) Calloc(length(_counts), hsize_t);

    int v = 1;
    for (i = 0; i < length(_counts); i++) {
    	v *= counts[i];
	_h_offsets[i] = offsets[i];
	_h_counts[i]  = counts[i];
    }

    space = _h5R_get_space(h5_dataset);
    H5Sselect_hyperslab(space, H5S_SELECT_SET, _h_offsets, NULL, _h_counts, NULL);
    memspace = H5Screate_simple(length(_counts), _h_counts, NULL);

    switch (INTEGER(h5R_get_type(h5_dataset))[0]) {
    case H5T_INTEGER: 
	PROTECT(dta = allocVector(INTSXP, v));
	memtype = H5T_NATIVE_INT;
	buf = INTEGER(dta);
	break;
    case H5T_FLOAT:
	PROTECT(dta = allocVector(REALSXP, v));
	memtype = H5T_NATIVE_DOUBLE;
	buf = REAL(dta);
	break;
    case H5T_STRING:
	buf = (char **) Calloc(v, char*);
	memtype = H5Tcopy (H5T_C_S1);
	H5Tset_size (memtype, H5T_VARIABLE);   
	break;
    default:
	error("Unsupported class in %s\n", __func__);
    }
    
    H5Dread(HID(h5_dataset), memtype, memspace, space, H5P_DEFAULT, buf);

    /** There requires a little more with strings. **/
    if (H5T_STRING == INTEGER(h5R_get_type(h5_dataset))[0]) {
	PROTECT(dta = allocVector(STRSXP, v));
	for (i = 0; i < v; i++)
	    if (((char **) buf)[i]) {
		SET_STRING_ELT(dta, i, mkChar(  ((char **) buf)[i] )); 
	    }

	H5Dvlen_reclaim (memtype, memspace, H5P_DEFAULT, buf);
	
	H5Tclose(memtype);
	Free(buf);
    }

    /** clean up. **/
    Free(_h_offsets);
    Free(_h_counts);
    H5Sclose(memspace);
    H5Sclose(space);

    UNPROTECT(1);
    
    return dta;
}

  
SEXP h5R_read_attr(SEXP h5_attr) {
    SEXP dta = R_NilValue;
    hid_t memtype = -1;
    void* buf = NULL; 

    switch (INTEGER(h5R_get_type(h5_attr))[0]) {
    case H5T_INTEGER:
	PROTECT(dta = allocVector(INTSXP, _h5R_get_nelts(h5_attr)));
	buf = INTEGER(dta);
	memtype = H5T_NATIVE_INT;
	break;
    case H5T_FLOAT:
	PROTECT(dta = allocVector(REALSXP, _h5R_get_nelts(h5_attr)));
	buf = REAL(dta);
	memtype = H5T_NATIVE_DOUBLE;
	break;
    case H5T_STRING:
	return _h5R_read_vlen_str(h5_attr);
    default:
	error("Unsupported class in %s.\n", __func__);
    }

    H5Aread(HID(h5_attr), memtype, buf);
    UNPROTECT(1);

    return dta;
}

/**
 * File content iteration.
 */
typedef struct __index_and_SEXP__ {
    int  i;
    SEXP s;
} index_and_SEXP;


herr_t _h5R_count_func(hid_t loc_id, const char *name, const H5O_info_t *info,
		       void *operator_data) {
    int* counter = ((int *) operator_data);
    (*counter)++;
    
    return 0;
}

herr_t _h5R_capture_name(hid_t loc_id, const char *name, const H5O_info_t *info,
			 void *operator_data) {
    index_and_SEXP* od = (index_and_SEXP*) operator_data;
    SET_STRING_ELT(od->s, (od->i)++, mkChar(name));
    
    return 0;
}

herr_t _h5R_capture_name_and_type(hid_t loc_id, const char *name, const H5O_info_t *info,
				  void *operator_data) {
    index_and_SEXP* od = (index_and_SEXP*) operator_data;
    SEXP lst, str;

    PROTECT(lst = allocVector(VECSXP, 2));
    PROTECT(str = allocVector(STRSXP, 1));
    SET_STRING_ELT(str, 0, mkChar(name));
    SET_VECTOR_ELT(lst, 0, str);
    SET_VECTOR_ELT(lst, 1, ScalarInteger(info->type));
    SET_VECTOR_ELT(od->s, (od->i)++, lst);

    UNPROTECT(2);

    return 0;
}

SEXP h5R_list_contents(SEXP h5_obj) {
    int counter = 0; 
    index_and_SEXP* isxp = (index_and_SEXP*) Calloc(1, index_and_SEXP);
    SEXP dta;

    H5Ovisit (HID(h5_obj), H5_INDEX_NAME, H5_ITER_NATIVE, _h5R_count_func, (void*) &counter);

    PROTECT(dta = allocVector(VECSXP, counter));
    isxp->s = dta;
    isxp->i = 0;
    H5Ovisit (HID(h5_obj), H5_INDEX_NAME, H5_ITER_NATIVE, _h5R_capture_name_and_type, (void*) isxp);

    Free(isxp);
    UNPROTECT(1);

    return(dta);
}

SEXP h5R_list_attributes(SEXP h5_obj) {
    int counter = 0;
    hsize_t n   = 0;
    SEXP dta;

    H5Aiterate(HID(h5_obj), H5_INDEX_NAME, H5_ITER_NATIVE, &n, (H5A_operator2_t) _h5R_count_func, (void*) &counter);
    
    index_and_SEXP* isxp = (index_and_SEXP*) Calloc(1, index_and_SEXP);
    PROTECT(dta = allocVector(STRSXP, counter));
    isxp->s = dta;
    isxp->i = 0;
    
    n = 0;
    H5Aiterate(HID(h5_obj), H5_INDEX_NAME, H5_ITER_NATIVE, &n, (H5A_operator2_t) _h5R_capture_name, (void*) isxp);

    Free(isxp);
    UNPROTECT(1);

    return(dta);
}
