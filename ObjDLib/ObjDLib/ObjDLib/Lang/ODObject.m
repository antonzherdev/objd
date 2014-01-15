#import "ODObject.h"
#import "CNRange.h"
#import "CNTypes.h"
#import "CNOption.h"

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


ODPType * odByteType() {
    static ODPType * type = nil;
    if(type == nil) {
        type = [ODPType typeWithCls:[NSNumber class] name:@"byte" size:1 wrap:^id(void *pVoid, NSUInteger i) {
            return numb(((char*)pVoid)[i]);
        }];
    }
    return type;
}

ODPType * odInt4Type() {
    static ODPType * type = nil;
    if(type == nil) {
        type = [ODPType typeWithCls:[NSNumber class] name:@"int4" size:4 wrap:^id(void *pVoid, NSUInteger i) {
            return numi4(((int*)pVoid)[i]);
        }];
    }
    return type;
}
ODPType * oduInt4Type() {
    static ODPType * type = nil;
    if(type == nil) {
        type = [ODPType typeWithCls:[NSNumber class] name:@"uint4" size:4 wrap:^id(void *pVoid, NSUInteger i) {
            return numui4(((unsigned int*)pVoid)[i]);
        }];
    }
    return type;
}

ODPType * odFloat4Type() {
    static ODPType * type = nil;
    if(type == nil) {
        type = [ODPType typeWithCls:[NSNumber class] name:@"float4" size:4 wrap:^id(void *pVoid, NSUInteger i) {
            return numf4(((float*)pVoid)[i]);
        }];
    }
    return type;
}

@implementation NSObject(ODObject)
+ (id)object {
    return [NSObject new];
}
@end

@implementation ODObject

+ (id)asKindOfClass:(Class)pClass object:(id)obj{
    return [obj isKindOfClass:pClass] ? [CNSome someWithValue:obj] : [CNOption none];
}

+ (id)asKindOfProtocol:(Protocol *)protocol object:(id)obj {
    return [obj conformsToProtocol:protocol] ? [CNSome someWithValue:obj] : [CNOption none];
}
@end