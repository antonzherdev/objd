#import "CNObject.h"
#import "CNRange.h"
#import "CNTypes.h"

@implementation NSNumber (ODObject)
- (NSInteger)compareTo:(id)to {
    return [self compare:to];
}
@end

CNRange* intRange(NSInteger x) {
    return [CNRange rangeWithStart:0 end:x - 1 step:1];
}
CNRange* intTo(NSInteger a, NSInteger b) {
    return [CNRange rangeWithStart:a end:b step:(a <= b) ? 1 : -1];
}

CNRange* uintRange(NSUInteger x) {
    return [CNRange rangeWithStart:0 end:x - 1 step:1];
}

CNRange* uintTo(NSUInteger a, NSUInteger b) {
    return [CNRange rangeWithStart:a end:b step:(a <= b) ? 1 : -1];
}


CNPType * cnByteType() {
    static CNPType * type = nil;
    if(type == nil) {
        type = [CNPType typeWithCls:[NSNumber class] name:@"byte" size:1 wrap:^id(void *pVoid, NSUInteger i) {
            return numb(((char *) pVoid)[i]);
        }];
    }
    return type;
}

CNPType * cnInt4Type() {
    static CNPType * type = nil;
    if(type == nil) {
        type = [CNPType typeWithCls:[NSNumber class] name:@"int4" size:4 wrap:^id(void *pVoid, NSUInteger i) {
            return numi4(((int *) pVoid)[i]);
        }];
    }
    return type;
}
CNPType * cnuInt4Type() {
    static CNPType * type = nil;
    if(type == nil) {
        type = [CNPType typeWithCls:[NSNumber class] name:@"uint4" size:4 wrap:^id(void *pVoid, NSUInteger i) {
            return numui4(((unsigned int *) pVoid)[i]);
        }];
    }
    return type;
}

CNPType * cnFloat4Type() {
    static CNPType * type = nil;
    if(type == nil) {
        type = [CNPType typeWithCls:[NSNumber class] name:@"float4" size:4 wrap:^id(void *pVoid, NSUInteger i) {
            return numf4(((float *) pVoid)[i]);
        }];
    }
    return type;
}

@implementation NSObject(ODObject)
+ (id)object {
    return [NSObject new];
}
@end

@implementation CNObject

+ (id)asKindOfClass:(Class)pClass object:(id)obj{
    return [obj isKindOfClass:pClass] ? obj : nil;
}

+ (id)asKindOfProtocol:(Protocol *)protocol object:(id)obj {
    return [obj conformsToProtocol:protocol] ? obj : nil;
}
@end