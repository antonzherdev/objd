#import "NSSet+CNChain.h"
#import "CNChain.h"
#import "CNOption.h"
#import "CNEnumerator.h"


@implementation NSSet (CNChain)
- (id <CNIterator>)iterator {
    return [CNEnumerator enumeratorWithEnumerator:[self objectEnumerator]];
}

- (id)head {
    return [self anyObject];
}

- (id)headOpt {
    if(self.count == 0) return [CNOption none];
    return [CNOption someValue:[self anyObject]];
}

- (BOOL)isEmpty {
    return self.count == 0;
}

- (CNChain *)chain {
    return [CNChain chainWithCollection:self];
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

- (id)findWhere:(BOOL(^)(id))where {
    id ret = [CNOption none];
    for(id item in self)  {
        if(where(item)) {
            ret = item;
            break;
        }
    }
    return ret;
}

- (id)convertWithBuilder:(id<CNBuilder>)builder {
    for(id x in self)  {
        [builder appendItem:x];
    }
    return [builder build];
}


- (void)forEach:(cnP)p {
    for(id item in self)  {
        p(item);
    }
}

- (BOOL)goOn:(BOOL (^)(id))on {
    for(id item in self)  {
        if(!on(item)) return NO;
    }
    return YES;
}

- (BOOL)containsItem:(id)item {
    return [self containsObject:item];
}

@end