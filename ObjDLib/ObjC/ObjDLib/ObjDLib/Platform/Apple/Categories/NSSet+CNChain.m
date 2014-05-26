#import "NSSet+CNChain.h"
#import "CNChain.h"
#import "CNEnumerator.h"
#import "CNDispatchQueue.h"


@implementation NSSet (CNChain)
+ (CNType *)type {
    static CNClassType* __type = nil;
    if(__type == nil) __type = [CNClassType classTypeWithCls:[NSSet class]];
    return nil;
}

- (CNType*) type {
    return [NSSet type];
}


- (id <CNIterator>)iterator {
    return [CNEnumerator enumeratorWithEnumerator:[self objectEnumerator]];
}

- (id)head {
    if(self.count == 0) return nil;
    return uwrapNil([self anyObject]);
}

- (BOOL)isEmpty {
    return self.count == 0;
}

- (CNChain *)chain {
    return [CNChain applyCollection:self];
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    BOOL ret = NO;
    for(id item in self)  {
        if(where(uwrapNil(item))) {
            ret = YES;
            break;
        }
    }
    return ret;
}

- (BOOL)allConfirm:(BOOL(^)(id))confirm {
    BOOL ret = YES;
    for(id item in self)  {
        if(!confirm(item)) {
            ret = NO;
            break;
        }
    }
    return ret;
}

- (id)findWhere:(BOOL(^)(id))where {
    id ret = nil;
    for(id item in self)  {
        if(where(uwrapNil(item))) {
            ret = item;
            break;
        }
    }
    return ret;
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    for(id x in self)  {
        [builder appendItem:uwrapNil(x)];
    }
    return [builder build];
}


- (void)forEach:(cnP)p {
    for(id item in self)  {
        p(uwrapNil(item));
    }
}

- (void)parForEach:(void (^)(id))each {
    for(id item in self)  {
        [[CNDispatchQueue aDefault] asyncF:^{
            each(uwrapNil(item));
        }];
    }
}


- (CNGoR)goOn:(CNGoR (^)(id))on {
    for(id item in self)  {
        if(on(uwrapNil(item)) == CNGo_Break) return CNGo_Break;
    }
    return CNGo_Continue;
}

- (BOOL)containsItem:(id)item {
    return [self containsObject:wrapNil(item)];
}

- (id <CNMSet>)mCopy {
    return (id <CNMSet>) [self mutableCopy];
}

- (BOOL)isEqualIterable:(id<CNIterable>)iterable {
    if([self count] == [iterable count]) {
        return YES;
    } else {
        id<CNIterator> ai = [self iterator];
        id<CNIterator> bi = [iterable iterator];
        while([ai hasNext] && [bi hasNext]) {
            if(!([[ai next] isEqual:[bi next]])) return NO;
        }
        return YES;
    }
}


@end