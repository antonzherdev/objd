#import "CNType.h"


typedef void* Pointer;


static inline Pointer cnPointerCopyBytes(Pointer p, NSUInteger bytes) {
    Pointer d = malloc(bytes);
    memcpy(d, p, bytes);
    return d;
}

static inline void cnPointerFree(Pointer p) {
    free(p);
}

static inline Pointer cnPointerApplyBytes(NSUInteger bytes) {
    return calloc(bytes, 1);
}

static inline Pointer cnPointerApplyBytesCount(NSUInteger bytes, NSUInteger count) {
    return calloc(bytes, count);
}

static inline Pointer cnPointerApplyTp(CNPType * tp) {
    return calloc(tp.size, 1);
}

static inline Pointer cnPointerApplyTpCount(CNPType * tp, NSUInteger count) {
    return calloc(tp.size, count);
}
