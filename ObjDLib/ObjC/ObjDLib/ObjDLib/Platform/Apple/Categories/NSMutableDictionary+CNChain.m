#import "chain.h"


@implementation NSMutableDictionary (CNChain)
- (void)setKey:(id)key value:(id)value {
    [self setObject:wrapNil(value) forKey:wrapNil(key)];
}

- (id)removeKey:(id)key {
    id ret = [self objectForKey:wrapNil(key)];
    if(ret != nil) [self removeObjectForKey:wrapNil(key)];
    return ret;
}

+ (NSMutableDictionary *)hashMap {
    return [NSMutableDictionary dictionary];
}

- (id)applyKey:(id)key orUpdateWith:(id (^)())with {
    id v = [self objectForKey:wrapNil(key)];
    if(v != nil) return v;
    v = with();
    [self setObject:wrapNil(v) forKey:wrapNil(key)];
    return v;
}


- (id)modifyKey:(id)key by:(id(^)(id))by {
    id v = by(self[wrapNil(key)]);
    if(v == nil) {
        [self removeObjectForKey:wrapNil(key)];
    } else {
        [self setObject:v forKey:wrapNil(key)];
    }
    return v;
}

- (void)appendItem:(CNTuple *)object {
    [self setObject:wrapNil(object.b) forKey:wrapNil(object.a)];
}

- (BOOL)removeItem:(CNTuple *)object {
    NSUInteger oldCount = self.count;
    [self removeObjectForKey:wrapNil(object.a)];
    return oldCount > self.count;
}

- (NSDictionary*)im {
    return self;
}

- (NSDictionary*)imCopy {
    return [NSDictionary dictionaryWithDictionary:self];
}

- (void)assignImMap:(id <CNImMap>)imMap {
    if([imMap isKindOfClass:[NSDictionary class]]) {
        [self setDictionary:(NSDictionary *) imMap];
    } else {
        [self removeAllObjects];
        [imMap forEach:^void(CNTuple* _) {
            [self setObject:wrapNil(_.b) forKey:wrapNil(_.a)];
        }];
    }
}


- (void)mutableFilterBy:(BOOL(^)(id))by {
    @throw @"Hasn't implemented yet";
}


- (void)clear {
    [self removeAllObjects];
}

- (id <CNMIterator>)mutableIterator {
    @throw @"Hasn't implemented yet";
}
@end