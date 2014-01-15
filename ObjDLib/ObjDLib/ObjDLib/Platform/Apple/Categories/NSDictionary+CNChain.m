#import "NSDictionary+CNChain.h"
#import "NSArray+CNChain.h"
#import "CNChain.h"
#import "CNOption.h"


@implementation NSDictionary (CNChain)
- (id <CNIterable>)values {
    return [self allValues];
}
- (NSDictionary *)dictionaryByAddingValue:(id)value forKey:(id)key {
    NSMutableDictionary * ret = [NSMutableDictionary dictionaryWithDictionary:self];
    [ret setObject:key forKey:key];
    return ret;
}

- (id)applyKey:(id)key {
    id ret = self[key];
    if(ret == nil) @throw [NSString stringWithFormat:@"No value for key %@", key];
    return ret;
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    __block BOOL ret = NO;
    [self goOn:^BOOL(id x) {
        if(where(numb(ret))) {
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
        if(!confirm(numb(ret))) {
            ret = NO;
            return NO;
        } else {
            return YES;
        }
    }];
    return ret;
}


- (id)optKey:(id)key {
    id ret = self[key];
    return ret == nil ? [CNOption none] : [CNSome someWithValue:ret];
}

- (CNChain *)chain {
    return [CNChain chainWithCollection:self];
}

- (void)forEach:(cnP)p {
    [self enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        p(tuple(key, obj));
    }];
}

- (BOOL)goOn:(BOOL(^)(id))on {
    __block BOOL ret = YES;
    [self enumerateKeysAndObjectsUsingBlock:^(id key, id obj, BOOL *stop) {
        if(!on(tuple(key, obj))) {
            ret = NO;
            *stop = YES;
        }
    }];
    return ret;
}

- (BOOL)containsItem:(id)item {
    return [[self allValues] containsObject:item];
}


- (id <CNIterator>)iterator {
    @throw @"Hasn't implemented yet";
}

- (BOOL)containsKey:(id)key {
    return [self objectForKey:key] != nil;
}

- (BOOL)isValueEqualKey:(id)key value:(id)value {
    id v = [self objectForKey:key];
    return v != nil && [v isEqual:value];
}


- (id <CNIterable>)keys {
    return [self allKeys];
}

- (id)head {
    return [[self iterator] next];
}

- (id)headOpt {
    if(![self isEmpty]) return [CNOption applyValue:[[self iterator] next]];
    else return [CNOption none];
}


- (id)findWhere:(BOOL(^)(id))where {
    __block id ret = [CNOption none];
    [self goOn:^BOOL(id x) {
        if(where(ret)) {
            ret = [CNOption applyValue:x];
            NO;
        }
        return YES;
    }];
    return ret;
}

- (id)convertWithBuilder:(id <CNBuilder>)builder {
    [self forEach:^void(id x) {
        [builder appendItem:x];
    }];
    return [builder build];
}

- (BOOL)isEmpty {
    return self.count == 0;
}

- (id<CNMap>)addItem:(CNTuple*)item {
    CNHashMapBuilder* builder = [CNHashMapBuilder hashMapBuilder];
    [builder appendAllItems:self];
    [builder appendItem:item];
    return [builder build];
}
@end