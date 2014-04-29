#import "CNYield.h"
#import "CNTuple.h"
#import <libkern/OSAtomic.h>


typedef void* VoidRef;
@class CNChain;
typedef void (^cnChainBuildBlock)(CNChain * chain);
typedef BOOL (^cnPredicate)(id x);
typedef NSInteger (^cnCompare)(id x, id y);
typedef id (^cnF)(id x);
typedef id (^cnF2)(id x, id y);
typedef id (^cnF3)(id x, id y, id z);
typedef id (^cnF0)();
typedef void (^cnP)(id x);
typedef void (^cnP2)(id x, id y);

extern id cnResolveCollection(id collection);


#define numc(expr) [NSNumber numberWithChar:expr]
#define unumc(expr) [expr charValue]
#define numi4(expr) [NSNumber numberWithInt:expr]
#define unumi4(expr) [expr intValue]
#define numi8(expr) [NSNumber numberWithLong:expr]
#define unumi8(expr) [expr longValue]
#define numi(expr) [NSNumber numberWithInteger:expr]
#define unumi(expr) [expr integerValue]

#define numuc(expr) [NSNumber numberWithUnsignedChar:expr]
#define unumuc(expr) [expr unsignedCharValue]
#define numui4(expr) [NSNumber numberWithUnsignedInt:expr]
#define unumui4(expr) [expr unsignedIntValue]
#define numui8(expr) [NSNumber numberWithUnsignedLong:expr]
#define unumui8(expr) [expr unsignedLongValue]
#define numui(expr) [NSNumber numberWithUnsignedInteger:expr]
#define unumui(expr) [expr unsignedIntegerValue]


#define numf4(expr) [NSNumber numberWithFloat:expr]
#define unumf4(expr) [expr floatValue]
#define numf8(expr) [NSNumber numberWithDouble:expr]
#define unumf8(expr) [expr doubleValue]

#if defined(__LP64__) && __LP64__
#define numf(expr) [NSNumber numberWithDouble:expr]
#define unumf(expr) [expr doubleValue]
#else
#define numf(expr) [NSNumber numberWithFloat:expr]
#define unumf(expr) [expr floatValue]
#endif

#define nums(expr) [NSNumber numberWithUnsignedShort:expr]
#define unums(expr) [expr unsignedShortValue]

#define numb(expr) [NSNumber numberWithBool:expr]
#define unumb(expr) [expr boolValue]

#define val(expr) \
        ({ \
            __typeof__(expr) chainReservedPrefix_lVar = expr; \
            [NSValue valueWithBytes:&chainReservedPrefix_lVar objCType:@encode(__typeof__(expr))]; \
        })
#define cnVoidRefApplyI(expr) (VoidRef)(expr)

#define voidRef(expr) \
        ({ \
            __typeof__(expr) chainReservedPrefix_lVar = expr; \
            (void*)&chainReservedPrefix_lVar; \
        })

#define uval(tp, expr) \
        ({ \
            tp chainReservedPrefix_uval; \
            [expr getValue:&chainReservedPrefix_uval]; \
            chainReservedPrefix_uval; \
        })

#define wrap(tp, expr) [tp ## Wrap wrapWithValue:expr]

#define uwrap(tp, expr) [((tp ## Wrap*)expr) value]


#define arr(p_type, p_f, p_count)  CNPArray applyStride:sizeof(p_type) wrap:^id(Pointer arr, NSUInteger i) { \
    return p_f(((p_type*)(arr))[i]);\
} count:p_count copyBytes:(p_type[])
#define arrp(p_type, p_f, p_count)  CNPArray applyStride:sizeof(p_type) wrap:^id(Pointer arr, NSUInteger i) { \
    return p_f(((p_type*)(arr))[i]);\
} count:p_count copyBytes:(p_type*)
#define arrc(p_count) arr(char, numc, p_count)
#define arruc(p_count) arr(unsigned char, numuc, p_count)
#define arri(p_count) arr(NSInteger, numi, p_count)
#define arrui(p_count) arr(NSUInteger, numui, p_count)
#define arri4(p_count) arr(int, numi4, p_count)
#define arrui4(p_count) arr(unsigned int, numui4, p_count)
#define arri8(p_count) arr(long, numi8, p_count)
#define arrui8(p_count) arr(unsigned long, numui8, p_count)
#define arrf(p_count) arr(CGFloat, numf, p_count)
#define arrf4(p_count) arr(float, numf4, p_count)
#define arrf8(p_count) arr(double, numf8, p_count)
#define arrs(p_type, p_count) CNPArray applyStride:sizeof(p_type) wrap:^id(Pointer arr, NSUInteger i) { \
    return wrap(p_type, ((p_type*)(arr))[i]);\
} count:p_count copyBytes:(p_type[])
#define arrsv(p_type, p_count) CNPArray applyStride:sizeof(p_type) wrap:^id(Pointer arr, NSUInteger i) { \
    return wrap(p_type, ((p_type*)(arr))[i]);\
} count:p_count copyBytes:(p_type*)

static inline NSUInteger VoidRefHash(void * v) {
    return (NSUInteger) v;
}

static inline BOOL VoidRefEq(void * a, void * b) {
    return a == b;
}

static inline NSString* VoidRefDescription(void * v) {
    return [NSString stringWithFormat:@"%p", v];
}

static inline void* copy(void * mem, NSUInteger len) {
    void* ret = malloc(len);
    memcpy(ret, mem, len);
    return ret;
}



static inline CNTuple* tuple(id anA, id aB) {
    return [CNTuple tupleWithA:anA b: aB];
}
static inline CNTuple3* tuple3(id anA, id aB, id aC) {
    return [CNTuple3 tuple3WithA:anA b: aB c:aC];
}
static inline CNTuple4* tuple4(id anA, id aB, id aC, id aD) {
    return [CNTuple4 tuple4WithA:anA b: aB c:aC d:aD];
}
static inline CNTuple5* tuple5(id anA, id aB, id aC, id aD, id aE) {
    return [CNTuple5 tuple5WithA:anA b: aB c:aC d:aD e:aE];
}

#define autoreleasePoolStart() @autoreleasepool {
#define autoreleasePoolEnd() }

#define memoryBarrier() OSMemoryBarrier()

static inline id nonnil(id value) {
    if(!value) @throw @"Null pointer exception";
    return value;
}

static inline id wrapNil(id object) {
    if(object == nil) {
        static id null;
        if(null == nil) null = [NSNull null];
        return null;
    } else return object;
}
static inline id uwrapNil(id object) {
    static id null;
    if(null == nil) null = [NSNull null];
    return object == null ? nil : object;
}
