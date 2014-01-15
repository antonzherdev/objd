#import "NSMutableSet+CNChain.h"
#import "NSSet+CNChain.h"
#import "CNEnumerator.h"


@implementation NSMutableSet (CNChain)
+ (id)mutableSet {
    return [NSMutableSet set];
}

- (void)appendItem:(id)object {
    [self addObject:object];
}

- (void)removeItem:(id)object {
    [self removeObject:object];
}

- (void)clear {
    [self removeAllObjects];
}

- (id <CNMutableIterator>)mutableIterator {
    return [CNMutableEnumerator enumeratorWithEnumerator:[self objectEnumerator]];
}
@end