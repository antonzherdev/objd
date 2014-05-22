#import "objd.h"
#import "CNSortBuilder.h"

#import "CNChain.h"
#import "CNPlat.h"
#import "CNCollection.h"
#import "CNType.h"
@implementation CNSortBuilder
static CNClassType* _CNSortBuilder_type;
@synthesize chain = _chain;

+ (instancetype)sortBuilderWithChain:(CNChain*)chain {
    return [[CNSortBuilder alloc] initWithChain:chain];
}

- (instancetype)initWithChain:(CNChain*)chain {
    self = [super init];
    if(self) {
        _chain = chain;
        _functions = [CNMArray array];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNSortBuilder class]) _CNSortBuilder_type = [CNClassType classTypeWithCls:[CNSortBuilder class]];
}

- (CNSortBuilder*)ascBy:(id(^)(id))by {
    [_functions appendItem:^NSInteger(id x, id y) {
        return [by(x) compareTo:by(y)];
    }];
    return self;
}

- (CNSortBuilder*)descBy:(id(^)(id))by {
    [_functions appendItem:^NSInteger(id x, id y) {
        return [by(y) compareTo:by(x)];
    }];
    return self;
}

- (CNSortBuilder*)andF:(NSInteger(^)(id, id))f {
    [_functions appendItem:f];
    return self;
}

- (CNChain*)endSort {
    return [_chain sortComparator:^NSInteger(id x, id y) {
        NSInteger ret = 0;
        id<CNIterator> i = [_functions iterator];
        while(ret == 0 && [i hasNext]) {
            NSInteger(^f)(id, id) = [i next];
            ret = f(x, y);
        }
        return ret;
    }];
}

- (CNClassType*)type {
    return [CNSortBuilder type];
}

+ (CNClassType*)type {
    return _CNSortBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"chain=%@", self.chain];
    [description appendString:@">"];
    return description;
}

@end

