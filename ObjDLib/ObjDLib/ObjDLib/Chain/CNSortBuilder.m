#import "objd.h"
#import "CNSortBuilder.h"

#import "CNChain.h"
#import "CNCollection.h"
#import "ODType.h"
@implementation CNSortBuilder{
    CNChain* _chain;
    NSMutableArray* _functions;
}
static ODClassType* _CNSortBuilder_type;
@synthesize chain = _chain;

+ (id)sortBuilderWithChain:(CNChain*)chain {
    return [[CNSortBuilder alloc] initWithChain:chain];
}

- (id)initWithChain:(CNChain*)chain {
    self = [super init];
    if(self) {
        _chain = chain;
        _functions = [NSMutableArray mutableArray];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNSortBuilder_type = [ODClassType classTypeWithCls:[CNSortBuilder class]];
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
    return [_chain sort:^NSInteger(id x, id y) {
        NSInteger ret = 0;
        id<CNIterator> i = [_functions iterator];
        while(ret == 0 && [i hasNext]) {
            NSInteger(^f)(id, id) = [i next];
            ret = f(x, y);
        }
        return ret;
    }];
}

- (ODClassType*)type {
    return [CNSortBuilder type];
}

+ (ODClassType*)type {
    return _CNSortBuilder_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNSortBuilder* o = ((CNSortBuilder*)(other));
    return [self.chain isEqual:o.chain];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.chain hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"chain=%@", self.chain];
    [description appendString:@">"];
    return description;
}

@end


