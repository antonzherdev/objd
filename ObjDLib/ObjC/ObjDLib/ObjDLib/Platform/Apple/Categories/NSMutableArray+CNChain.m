#import "NSMutableArray+CNChain.h"
#import "NSArray+CNChain.h"
#import "CNEnumerator.h"


@implementation NSMutableArray (CNChain)
+ (CNType *)type {
    static CNClassType* __type = nil;
    if(__type == nil) __type = [CNClassType classTypeWithCls:[NSMutableArray class]];
    return nil;
}

- (CNType*) type {
    return [NSMutableArray type];
}


+ (NSMutableArray *)mutableArray {
    return [NSMutableArray array];
}

- (void)appendItem:(id)object {
    [self addObject:wrapNil(object)];
}

+ (NSMutableArray *)applyCapacity:(NSUInteger)size {
    return [NSMutableArray arrayWithCapacity:size];
}

- (BOOL)removeItem:(id)object {
    NSUInteger oldCount = self.count;
    [self removeObject:wrapNil(object)];
    return oldCount > self.count;
}

- (void)mutableFilterBy:(BOOL(^)(id))by {
    NSMutableIndexSet* indexSet = [NSMutableIndexSet indexSet];
    NSUInteger i = 0;
    NSNull *n = [NSNull null];
    for(id item in self) {
        if(!by((item == n) ? nil : item)) {
            [indexSet addIndex:i];
        }
        i++;
    }
    [self removeObjectsAtIndexes:indexSet];
}


- (void)clear {
    [self removeAllObjects];
}

- (BOOL)removeIndex:(NSUInteger)index1 {
    NSUInteger oldCount = self.count;
    [self removeObjectAtIndex:index1];
    return oldCount > self.count;
}

- (void)insertIndex:(NSUInteger)index1 item:(id)item {
    [self insertObject:item atIndex:index1];
}


- (void)prependItem:(id)item {
    [self insertObject:wrapNil(item) atIndex:0];
}

- (void)setIndex:(NSUInteger)index1 item:(id)item {
    [self setObject:wrapNil(item) atIndexedSubscript:index1];
}

- (NSArray*)im {
    return self;
}

- (NSArray*)imCopy {
    return [NSArray arrayWithArray:self];
}


- (id <CNMIterator>)mutableIterator {
    return [CNMEnumerator enumeratorWithEnumerator:[self objectEnumerator]];
}
@end