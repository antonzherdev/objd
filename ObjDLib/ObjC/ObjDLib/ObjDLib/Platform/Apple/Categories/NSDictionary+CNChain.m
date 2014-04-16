#import "NSDictionary+CNChain.h"
#import "NSArray+CNChain.h"
#import "CNChain.h"
#import "CNDispatchQueue.h"


@implementation NSDictionary (CNChain)
- (id <CNIterable>)values {
    return [self allValues];
}
- (NSDictionary *)dictionaryByAddingValue:(id)value forKey:(id)key {
    NSMutableDictionary * ret = [NSMutableDictionary dictionaryWithDictionary:self];
    [ret setObject:wrapNil(value) forKey:wrapNil(key)];
    return ret;
}

- (id)applyKey:(id)key {
    id ret = self[key];
    if(ret == nil) @throw [NSString stringWithFormat:@"No value for key %@", key];
    return uwrapNil(ret);
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    __block BOOL ret = NO;
    [self goOn:^BOOL(id x) {
        if(where(uwrapNil(x))) {
            ret = YES;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}

- (BOOL)allConfirm:(BOOL(^)(id))confirm {
    __block BOOL ret = YES;
    [self goOn:^BOOL(id x) {
        if(!confirm(uwrapNil(x))) {
            ret = NO;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}


- (id)optKey:(id)key {
    return uwrapNil(self[key]);
}

- (id)getKey:(id)key orValue:(id)orValue {
    id ret = uwrapNil(self[key]);
    return ret == nil ? orValue : ret;
}


- (CNChain *)chain {
    return [CNChain chainWithCollection:self];
}

- (void)forEach:(cnP)p {
    [self enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        p(tuple(uwrapNil(key), uwrapNil(obj)));
    }];
}

- (void)parForEach:(cnP)p {
    [self enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        [[CNDispatchQueue aDefault] asyncF:^{
            p(tuple(uwrapNil(key), uwrapNil(obj)));
        }];
    }];
}


- (BOOL)goOn:(BOOL(^)(id))on {
    __block BOOL ret = YES;
    [self enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        if(!on(tuple(uwrapNil(key), uwrapNil(obj)))) {
            ret = NO;
            *stop = YES;
        }
    }];
    return ret;
}

- (BOOL)containsItem:(id)item {
    return [[self allValues] containsObject:wrapNil(item)];
}


- (id <CNIterator>)iterator {
    @throw @"Hasn't implemented yet";
}

- (BOOL)containsKey:(id)key {
    return [self objectForKey:wrapNil(key)] != nil;
}

- (BOOL)isValueEqualKey:(id)key value:(id)value {
    id v = uwrapNil([self objectForKey:wrapNil(key)]);
    return v != nil && [v isEqual:value];
}


- (id <CNIterable>)keys {
    return [self allKeys];
}

- (id)head {
    if(![self isEmpty]) return [[self iterator] next];
    else return nil;
}


- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = nil;
    [self goOn:^BOOL(id x) {
        if(where(uwrapNil(x))) {
            ret = x;
            NO;
        }
        return YES;
    }];
    return ret;
}

- (id)convertWithBuilder:(id <CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:uwrapNil(x)];
    }];
    return [builder build];
}

- (BOOL)isEmpty {
    return self.count == 0;
}

- (id <CNImMap>)addItem:(CNTuple*)item {
    CNHashMapBuilder* builder = [CNHashMapBuilder hashMapBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
}

- (id <CNMIterable>)mCopy {
    return (id <CNMIterable>) [self mutableCopy];
}

@end