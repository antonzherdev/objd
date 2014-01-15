#import "chain.h"


@implementation NSMutableDictionary (CNChain)
- (void)setKey:(id)key value:(id)value {
    [self setObject:value forKey:key];
}

- (id)removeForKey:(id)key {
    id ret = [self optKey:key];
    if([ret isDefined]) [self removeObjectForKey:key];
    return ret;
}

+ (NSMutableDictionary *)mutableDictionary {
    return [NSMutableDictionary dictionary];
}

- (id)objectForKey:(id)key orUpdateWith:(id (^)())with {
    id v = [self objectForKey:key];
    if(v != nil) return v;
    v = with();
    [self setObject:v forKey:key];
    return v;
}

- (id)modifyBy:(id (^)(id))with forKey:(id)key {
    id v = with([self optKey:key]);
    if([v isEmpty]) {
        [self removeObjectForKey:v];
    } else {
        [self setObject:[v get] forKey:key];
    }
    return v;
}

- (void)appendItem:(CNTuple *)object {
    [self setObject:object.b forKey:object.a];
}

- (void)removeItem:(CNTuple *)object {
    [self removeObjectForKey:object.a];
}

- (void)clear {
    [self removeAllObjects];
}

- (id <CNMutableIterator>)mutableIterator {
    @throw @"Hasn't implemented yet";
}


@end