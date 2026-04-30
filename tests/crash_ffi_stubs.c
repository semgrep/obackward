#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>

/* Reading at address 0 segfaults on every modern OS. We take the address from
   OCaml so the C compiler can't see it's statically NULL at compile time and
   replace the load with __builtin_trap or elide it under UB optimization.
   `noinline` keeps this function as its own frame in the captured stack trace
   so the test can confirm symbolization is wired up end-to-end. */
__attribute__((noinline))
CAMLprim value test_crash_ffi_null_deref(value v_addr) {
    CAMLparam1(v_addr);
    volatile char *p = (volatile char *)(uintptr_t)Long_val(v_addr);
    char c = *p;
    CAMLreturn(Val_int(c));
}
