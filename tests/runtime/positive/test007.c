/* Prologue and epilogue */

#include <juvix/api.h>

#define JUVIX_DECL_ARGS __attribute__((unused)) DECL_ARG(0)

#define JUVIX_DECL_DISPATCH                   \
    DECL_DISPATCH(0, juvix_dispatch_label_0); \
    juvix_dispatch_label_0:                   \
    ARG(0) = *juvix_ccl_sp;                   \
    DISPATCH(juvix_dispatch_label);

int main() {
    JUVIX_PROLOGUE(1, JUVIX_DECL_ARGS, JUVIX_DECL_DISPATCH);
    juvix_result = make_smallint(789);
    JUVIX_EPILOGUE;
    return 0;
}
