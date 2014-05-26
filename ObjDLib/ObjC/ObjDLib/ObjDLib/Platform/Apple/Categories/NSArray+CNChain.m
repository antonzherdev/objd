#import "CNCollection.h"
#import "NSArray+CNChain.h"
#import "NSSet+CNChain.h"
#import "CNChain.h"
#import "CNEnumerator.h"
#import "CNDispatchQueue.h"


@implementation NSArray (CNChain)
- (id)chain:(void (^)(CNChain *))block {
    CNChain *chain = [CNChain applyCollection:self];
    block(chain);
    return [chain toArray];
}

- (CNChain *)chain {
    return [CNChain applyCollection:self];
}

- (id <CNIterator>)iterator {
    return [CNEnumerator enumeratorWithEnumerator:[self objectEnumerator]];
}

- (BOOL)isEmpty {
    return self.count == 0;
}

- (id)head {
    if(self.count == 0) return nil;
    return uwrapNil([self objectAtIndex :0]);
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    for(id x in self)  {
        [builder appendItem:x];
    }
    return [builder build];
}


- (id)optIndex:(NSUInteger)index {
    if(index >= self.count) return nil;
    return [self objectAtIndex:index];
}

- (id)randomItem {
    if([self isEmpty]) return nil;
    else return [self objectAtIndex:cnuIntRndMax([self count] - 1)];
}

- (id)findWhere:(BOOL(^)(id))where {
    id ret = nil;
    for(id item in self)  {
        if(where(item)) {
            ret = item;
            break;
        }
    }
    return ret;
}

- (BOOL)existsWhere:(BOOL(^)(id))where {
    BOOL ret = NO;
    for(id item in self)  {
        if(where(item)) {
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


- (void)forEach:(cnP)p {
    for(id item in self)  {
        p(item);
    }
}

- (void)parForEach:(void (^)(id))each {
    for(id item in self)  {
        [[CNDispatchQueue aDefault] asyncF:^{
            each(item);
        }];
    }
}


- (CNGoR)goOn:(CNGoR (^)(id))on {
    for(id item in self)  {
        if(!on(item) == CNGo_Break) return CNGo_Break;
    }
    return CNGo_Continue;
}

+ (CNType *)type {
    static CNClassType* __type = nil;
    if(__type == nil) __type = [CNClassType classTypeWithCls:[NSArray class]];
    return nil;
}

- (CNType*) type {
    return [NSArray type];
}

- (NSArray *)arrayByRemovingObject:(id)object {
    return [[[self chain] filterWhen:^BOOL(id x) {
        return ![x isEqual:object];
    }] toArray];
}

- (BOOL)isEqualSeq:(id<CNSeq>)seq {
    if([self count] != [seq count]) return NO;
    id<CNIterator> ia = [self iterator];
    id<CNIterator> ib = [seq iterator];
    while([ia hasNext] && [ib hasNext]) {
        if(!([[ia next] isEqual:[ib next]])) return NO;
    }
    return YES;
}

- (NSString*)description {
    return [[self chain] toStringStart:@"[" delimiter:@", " end:@"]"];
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other)) return NO;
    if([other conformsToProtocol:@protocol(CNSeq)]) return [self isEqualSeq:((id<CNSeq>)(other))];
    return NO;
}


- (id <CNSet>)toSet {
    return [NSSet setWithArray:self];
}

- (NSArray*)addItem:(id)item {
    return [self arrayByAddingObject:item];
}

- (NSArray*)addSeq:(id<CNSeq>)seq {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilderWithCapacity:(self.count + seq.count)];
    [builder appendAllItems:self];
    [builder appendAllItems:seq];
    return ((NSArray*)([builder build]));
}

- (id <CNMSeq>)mCopy {
    return (id <CNMSeq>) [self mutableCopy];
}

- (NSArray*)subItem:(id)item {
    return [self arrayByRemovingObject:item];
}


- (id <CNSeq>)arrayByAddingItem:(id)item {
    return [self arrayByAddingObject:item];
}

- (id <CNSeq>)arrayByRemovingItem:(id)item {
    return [self arrayByRemovingObject:item];
}

- (BOOL)containsItem:(id)item {
    return [self containsObject:item];
}


- (id)applyIndex:(NSUInteger)index {
    if(index >= self.count) @throw @"Incorrect index";
    return [self objectAtIndex:index];
}

- (NSArray*)tail {
    CNArrayBuilder* builder = [CNArrayBuilder arrayBuilderWithCapacity:self.count - 1];
    id<CNIterator> i = [self iterator];
    if([i hasNext]) {
        [i next];
        while([i hasNext]) {
            [builder appendItem:[i next]];
        }
    }
    return [builder build];
}

- (id)last {
    return [self lastObject];
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