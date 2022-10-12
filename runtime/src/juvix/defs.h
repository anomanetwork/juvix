#ifndef JUVIX_DEFS_H
#define JUVIX_DEFS_H

#include <juvix/config.h>

#ifdef API_LIBC
#include <stdio.h>
#include <stdlib.h>
#endif

// Number of bits in a word
#if defined(ARCH_WASM32) || defined(ARCH_NATIVE32)
#define BITS32
#elif defined(ARCH_NATIVE64)
#define BITS64
#else
#error "Unsupported configuration"
#endif

// Detect compiler
#ifdef __GNUC__
#ifdef __clang__
#define COMPILER_CLANG
#else
#define COMPILER_GCC
#endif
#endif

#if defined(COMPILER_CLANG) || defined(COMPILER_GCC)
#define EXT_LABELS_AS_VALUES
#endif

#if defined(COMPILER_GCC) || defined(COMPILER_CLANG)
#define likely(exp) (__builtin_expect((exp), 1))
#define unlikely(exp) (__builtin_expect((exp), 0))
#else
#define likely(exp) (exp)
#define unlikely(exp) (exp)
#endif

#if defined(COMPILER_CLANG) || defined(COMPILER_GCC)
#define UNREACHABLE __builtin_unreachable()
#else
#define UNREACHABLE \
    do {            \
    } while (0)
#endif

// typedefs for basic integer types
#if (defined(COMPILER_CLANG) || defined(COMPILER_GCC)) && !defined(API_LIBC)

typedef __SIZE_TYPE__ size_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINTPTR_TYPE__ uintptr_t;

#elif defined(API_LIBC)

#include <stdint.h>

#else
#error "Unsupported configuration"
#endif

typedef unsigned uint;

// typedefs for word_t, dword_t, int_t, long_t
#if defined(BITS32)

typedef uint32_t word_t;
typedef uint64_t dword_t;
typedef int32_t int_t;
typedef int64_t long_t;

#elif defined(BITS64)

typedef uint64_t word_t;
typedef int64_t int_t;

#ifdef __SIZEOF_INT128__
typedef unsigned __int128 dword_t;
typedef __int128 long_t;
#else
#error "Unsupported configuration"
#endif

#else
#error "Unsupported configuration"
#endif

#define PAGE_SIZE 65536
#define PAGE_MASK 0xffff

// NULL
#ifndef NULL
#define NULL ((void *)0)
#endif

#ifdef API_LIBC
#include <stdbool.h>
#else
typedef int bool;
#define true 1
#define false 0
#endif

#ifdef EXT_LABELS_AS_VALUES
#define LABEL_ADDR(label) &&label
#define STORED_GOTO(ptr) goto *(ptr)
typedef void *label_addr_t;
#else
#error \
    "The \"labels as values\" compiler extension is required (use GCC or clang)."
#endif

/**********************************************/
/* Basic primitive functions and macros */

static inline void print_msg(const char *msg) {
#if defined(API_LIBC)
    puts(msg);
#endif
}

static inline void error_exit() {
#if defined(API_LIBC)
    abort();
#elif defined(ARCH_WASM32)
    __builtin_trap();
#endif
}

static inline void error_exit_msg(const char *msg) {
    print_msg(msg);
    error_exit();
}

// Debug assertions
#ifdef DEBUG
#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)
#define ASSERT(x)                                                      \
    do {                                                               \
        if (!(x))                                                      \
            error_exit_msg(__FILE__ ":" TOSTRING(                      \
                __LINE__) ": Juvix assertion failed. Please report."); \
    } while (0)
#else
#define ASSERT(x) \
    do {          \
    } while (0)
#endif
#define ASSERT_EQ(a, b) ASSERT((a) == (b))

// Static assertions (requires C11)
#define STATIC_ASSERT(x) _Static_assert((x), "assertion failed")
#define STATIC_ASSERT_EQ(a, b) STATIC_ASSERT((a) == (b))

static inline size_t max(size_t a, size_t b) { return a < b ? b : a; }
// `alignment` must be a power of 2
static inline uintptr_t align(uintptr_t val, uintptr_t alignment) {
    return (val + alignment - 1) & ~(alignment - 1);
}
static inline void *palign(void *ptr, uintptr_t alignment) {
    return (void *)align((uintptr_t)ptr, alignment);
}
#define ASSERT_ALIGNED(x, y) \
    ASSERT((uintptr_t)(x) == align((uintptr_t)(x), (y)))

/*************************************************************************/
/* Static assertions */

#if defined(BITS32)
STATIC_ASSERT_EQ(sizeof(void *), 4);
#elif defined(BITS64)
STATIC_ASSERT_EQ(sizeof(void *), 8);
#else
#error "Unsupported configuration"
#endif

#endif
