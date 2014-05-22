#import "objd.h"
#import "CNGroupByLink.h"

#import "CNPlat.h"
#import "CNType.h"
@implementation CNImGroupByLink
static CNClassType* _CNImGroupByLink_type;
@synthesize factor = _factor;
@synthesize by = _by;
@synthesize start = _start;
@synthesize fold = _fold;

+ (instancetype)imGroupByLinkWithFactor:(CGFloat)factor by:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold {
    return [[CNImGroupByLink alloc] initWithFactor:factor by:by start:start fold:fold];
}

- (instancetype)initWithFactor:(CGFloat)factor by:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold {
    self = [super init];
    if(self) {
        _factor = factor;
        _by = [by copy];
        _start = [start copy];
        _fold = [fold copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNImGroupByLink class]) _CNImGroupByLink_type = [CNClassType classTypeWithCls:[CNImGroupByLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    CNMHashMap* m = [CNMHashMap hashMap];
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger _) {
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        id k = _by(item);
        id v = [m applyKey:k orUpdateWith:_start];
        [m setKey:k value:_fold(v, item)];
        return CNGo_Continue;
    } end:^CNGoR(CNGoR result) {
        if(result == CNGo_Break) return [yield endYieldWithResult:result];
        else return [yield yieldAllItems:m];
    }];
}

- (CNClassType*)type {
    return [CNImGroupByLink type];
}

+ (CNClassType*)type {
    return _CNImGroupByLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"factor=%f", self.factor];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNMGroupByLink
static CNClassType* _CNMGroupByLink_type;
@synthesize factor = _factor;
@synthesize by = _by;
@synthesize start = _start;
@synthesize append = _append;
@synthesize finish = _finish;

+ (instancetype)groupByLinkWithFactor:(CGFloat)factor by:(id(^)(id))by start:(id(^)())start append:(void(^)(id, id))append finish:(id(^)(id))finish {
    return [[CNMGroupByLink alloc] initWithFactor:factor by:by start:start append:append finish:finish];
}

- (instancetype)initWithFactor:(CGFloat)factor by:(id(^)(id))by start:(id(^)())start append:(void(^)(id, id))append finish:(id(^)(id))finish {
    self = [super init];
    if(self) {
        _factor = factor;
        _by = [by copy];
        _start = [start copy];
        _append = [append copy];
        _finish = [finish copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMGroupByLink class]) _CNMGroupByLink_type = [CNClassType classTypeWithCls:[CNMGroupByLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    CNMHashMap* m = [CNMHashMap hashMap];
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        return [yield beginYieldWithSize:((NSUInteger)(size * _factor))];
    } yield:^CNGoR(id item) {
        id k = _by(item);
        id v = [m applyKey:k orUpdateWith:_start];
        _append(v, item);
        return CNGo_Continue;
    } end:^CNGoR(CNGoR result) {
        if(result == CNGo_Break) return [yield endYieldWithResult:result];
        else return [yield endYieldWithResult:[m goOn:^CNGoR(CNTuple* t) {
            return [yield yieldItem:tuple(t.a, _finish(t.b))];
        }]];
    }];
}

- (CNClassType*)type {
    return [CNMGroupByLink type];
}

+ (CNClassType*)type {
    return _CNMGroupByLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"factor=%f", self.factor];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNDistinctLink
static CNClassType* _CNDistinctLink_type;
@synthesize factor = _factor;

+ (instancetype)distinctLinkWithFactor:(CGFloat)factor {
    return [[CNDistinctLink alloc] initWithFactor:factor];
}

- (instancetype)initWithFactor:(CGFloat)factor {
    self = [super init];
    if(self) _factor = factor;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNDistinctLink class]) _CNDistinctLink_type = [CNClassType classTypeWithCls:[CNDistinctLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    CNMHashSet* set = [CNMHashSet hashSet];
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        return [yield beginYieldWithSize:((NSUInteger)(size * _factor))];
    } yield:^CNGoR(id item) {
        if([set containsItem:item]) {
            return CNGo_Continue;
        } else {
            [set appendItem:item];
            return [yield yieldItem:item];
        }
    }];
}

- (CNClassType*)type {
    return [CNDistinctLink type];
}

+ (CNClassType*)type {
    return _CNDistinctLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"factor=%f", self.factor];
    [description appendString:@">"];
    return description;
}

@end

