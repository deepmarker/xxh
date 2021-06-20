#include <xxhash.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <uint32.h>
#include <uint64.h>
#include <uint128.h>

/* Common */

CAMLprim value xxh_version_stubs(value unit) {
    return Val_int(XXH_VERSION_NUMBER);
}


static struct custom_operations xxh3_state_ops = {
  "xxh3.state",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

#define State_val(v) (*((XXH3_state_t **) Data_custom_val(v)))

CAMLprim value xxh3_create_state_stubs (value unit) {
    CAMLparam1 (unit);
    CAMLlocal1(ret);
    XXH3_state_t *st = XXH3_createState();
    if (!st) {
        caml_failwith("XXH3_createState");
    };
    ret = caml_alloc_custom(&xxh3_state_ops, sizeof (XXH3_state_t *), 0, 1);
    State_val(ret) = st;
    CAMLreturn(ret);
}

CAMLprim value xxh3_free_state_stubs (value st) {
    XXH3_freeState(State_val(st));
    return Val_unit;
}

CAMLprim value xxh3_copy_state_stubs (value src) {
    CAMLparam1 (src);
    CAMLlocal1(ret);
    XXH3_state_t *st = XXH3_createState();
    if (!st) {
        caml_failwith("XXH3_createState");
    };
    XXH3_copyState(st, State_val(src));
    ret = caml_alloc_custom(&xxh3_state_ops, sizeof (XXH3_state_t *), 0, 1);
    State_val(ret) = st;
    CAMLreturn(ret);
}

/* XXH32 */

CAMLprim value xxh32_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    ret = copy_uint32(XXH32(Bytes_val(data), caml_string_length(data), Uint32_val(seed)));
    CAMLreturn(ret);
}

CAMLprim value xxh32_ba_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    ret = copy_uint32(XXH32(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0], Uint32_val(seed)));
    CAMLreturn(ret);
}

/* XXH64 */

CAMLprim value xxh64_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH64(Bytes_val(data), caml_string_length(data), Uint64_val(seed)));
    CAMLreturn(ret);
}

CAMLprim value xxh64_ba_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    ret = copy_uint32(XXH64(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0], Uint64_val(seed)));
    CAMLreturn(ret);
}

/* XXH3_64 */

CAMLprim value xxh3_64_stubs(value data) {
    CAMLparam1(data);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH3_64bits(Bytes_val(data), caml_string_length(data)));
    CAMLreturn(ret);
}

CAMLprim value xxh3_64_ba_stubs(value data) {
    CAMLparam1(data);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH3_64bits(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0]));
    CAMLreturn(ret);
}

CAMLprim value xxh3_64_seed_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH3_64bits_withSeed(Bytes_val(data), caml_string_length(data), Uint64_val(seed)));
    CAMLreturn(ret);
}

CAMLprim value xxh3_64_seed_ba_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH3_64bits_withSeed(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0], Uint64_val(seed)));
    CAMLreturn(ret);
}

CAMLprim value xxh3_64_secret_stubs(value data, value secret) {
    CAMLparam2(data, secret);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH3_64bits_withSecret(Bytes_val(data), caml_string_length(data),
                                                 Bytes_val(secret), caml_string_length(secret)));
    CAMLreturn(ret);
}

CAMLprim value xxh3_64_secret_ba_stubs(value data, value secret) {
    CAMLparam2(data, secret);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH3_64bits_withSecret(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0],
                                                 Caml_ba_data_val(secret), Caml_ba_array_val(secret)->dim[0]));
    CAMLreturn(ret);
}


CAMLprim value xxh3_64_reset_stubs (value st) {
    return Val_int(XXH3_64bits_reset(State_val(st)));
}

CAMLprim value xxh3_64_reset_seed_stubs (value st, value seed) {
    return Val_int(XXH3_64bits_reset_withSeed(State_val(st), Uint64_val(seed)));
}

CAMLprim value xxh3_64_reset_secret_stubs (value st, value secret) {
    return Val_int(XXH3_64bits_reset_withSecret(State_val(st), Bytes_val(secret), caml_string_length(secret)));
}

CAMLprim value xxh3_64_reset_secret_ba_stubs (value st, value secret) {
    return Val_int(XXH3_64bits_reset_withSecret(State_val(st), Caml_ba_data_val(secret), Caml_ba_array_val(secret)->dim[0]));
}

CAMLprim value xxh3_64_update_stubs (value st, value buf, value pos, value len) {
    return Val_int(XXH3_64bits_update(State_val(st), Bytes_val(buf)+Long_val(pos), Long_val(len)));
}

CAMLprim value xxh3_64_update_ba_stubs (value st, value buf, value pos, value len) {
    return Val_int(XXH3_64bits_update(State_val(st), Caml_ba_data_val(buf)+Long_val(pos), Long_val(len)));
}

CAMLprim value xxh3_64_digest_stubs (value st) {
    CAMLparam1(st);
    CAMLlocal1(ret);
    ret = copy_uint64(XXH3_64bits_digest(State_val(st)));
    CAMLreturn(ret);
}

/* XXH3_128 */

static inline value of_xxh128(XXH128_hash_t x) {
    uint128 ret;
#ifdef HAVE_UINT128
    ret = (__uint128_t)x.high64 << 64 | x.low64;
#else
    ret.high = x.high64;
    ret.low = x.low64;
#endif
    return copy_uint128(ret);
}

CAMLprim value xxh3_128_stubs(value data) {
    CAMLparam1(data);
    CAMLlocal1(ret);
    XXH128_hash_t x = XXH3_128bits(Bytes_val(data), caml_string_length(data));
    ret = of_xxh128(x);
    CAMLreturn(ret);
}

CAMLprim value xxh3_128_ba_stubs(value data) {
    CAMLparam1(data);
    CAMLlocal1(ret);
    XXH128_hash_t x = XXH3_128bits(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0]);
    ret = of_xxh128(x);
    CAMLreturn(ret);
}

CAMLprim value xxh3_128_seed_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    XXH128_hash_t x = XXH3_128bits_withSeed(Bytes_val(data), caml_string_length(data), Uint64_val(seed));
    ret = of_xxh128(x);
    CAMLreturn(ret);
}

CAMLprim value xxh3_128_seed_ba_stubs(value data, value seed) {
    CAMLparam2(data, seed);
    CAMLlocal1(ret);
    XXH128_hash_t x = XXH3_128bits_withSeed(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0], Uint64_val(seed));
    ret = of_xxh128(x);
    CAMLreturn(ret);
}

CAMLprim value xxh3_128_secret_stubs(value data, value secret) {
    CAMLparam2(data, secret);
    CAMLlocal1(ret);
    XXH128_hash_t x = XXH3_128bits_withSecret(Bytes_val(data), caml_string_length(data),
                                              Bytes_val(secret), caml_string_length(secret));
    ret = of_xxh128(x);
    CAMLreturn(ret);
}

CAMLprim value xxh3_128_secret_ba_stubs(value data, value secret) {
    CAMLparam2(data, secret);
    CAMLlocal1(ret);
    XXH128_hash_t x = XXH3_128bits_withSecret(Caml_ba_data_val(data), Caml_ba_array_val(data)->dim[0],
                                              Caml_ba_data_val(secret), Caml_ba_array_val(secret)->dim[0]);
    ret = of_xxh128(x);
    CAMLreturn(ret);
}

CAMLprim value xxh3_128_reset_stubs (value st) {
    return Val_int(XXH3_128bits_reset(State_val(st)));
}

CAMLprim value xxh3_128_reset_seed_stubs (value st, value seed) {
    return Val_int(XXH3_128bits_reset_withSeed(State_val(st), Uint64_val(seed)));
}

CAMLprim value xxh3_128_reset_secret_stubs (value st, value secret) {
    return Val_int(XXH3_128bits_reset_withSecret(State_val(st), Bytes_val(secret), caml_string_length(secret)));
}

CAMLprim value xxh3_128_reset_secret_ba_stubs (value st, value secret) {
    return Val_int(XXH3_128bits_reset_withSecret(State_val(st), Caml_ba_data_val(secret), Caml_ba_array_val(secret)->dim[0]));
}

CAMLprim value xxh3_128_update_stubs (value st, value buf, value pos, value len) {
    return Val_int(XXH3_128bits_update(State_val(st), Bytes_val(buf)+Long_val(pos), Long_val(len)));
}

CAMLprim value xxh3_128_update_ba_stubs (value st, value buf, value pos, value len) {
    return Val_int(XXH3_128bits_update(State_val(st), Caml_ba_data_val(buf)+Long_val(pos), Long_val(len)));
}

CAMLprim value xxh3_128_digest_stubs (value st) {
    CAMLparam1(st);
    CAMLlocal1(ret);
    XXH128_hash_t x = XXH3_128bits_digest(State_val(st));
    ret = of_xxh128(x);
    CAMLreturn(ret);
}
