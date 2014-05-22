#import "objd.h"
#import "CNMapLink.h"

#import "CNType.h"
@implementation CNMapLink
static CNClassType* _CNMapLink_type;
@synthesize f = _f;

+ (instancetype)mapLinkWithF:(id(^)(id))f {
    return [[CNMapLink alloc] initWithF:f];
}

- (instancetype)initWithF:(id(^)(id))f {
    self = [super init];
    if(self) _f = [f copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMapLink class]) _CNMapLink_type = [CNClassType classTypeWithCls:[CNMapLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield yield:^CNGoR(id item) {
        return [yield yieldItem:_f(item)];
    }];
}

- (CNClassType*)type {
    return [CNMapLink type];
}

+ (CNClassType*)type {
    return _CNMapLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNMapOptLink
static CNClassType* _CNMapOptLink_type;
@synthesize f = _f;

+ (instancetype)mapOptLinkWithF:(id(^)(id))f {
    return [[CNMapOptLink alloc] initWithF:f];
}

- (instancetype)initWithF:(id(^)(id))f {
    self = [super init];
    if(self) _f = [f copy];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNMapOptLink class]) _CNMapOptLink_type = [CNClassType classTypeWithCls:[CNMapOptLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield yield:^CNGoR(id item) {
        CNGoR __tmp_0r;
        {
            id _ = _f(item);
            if(_ != nil) __tmp_0r = [yield yieldItem:_];
            else __tmp_0r = CNGo_Nil;
        }
        if(__tmp_0r != CNGo_Nil) return ((CNGoR)(__tmp_0r));
        else return CNGo_Continue;
    }];
}

- (CNClassType*)type {
    return [CNMapOptLink type];
}

+ (CNClassType*)type {
    return _CNMapOptLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNFlatLink
static CNClassType* _CNFlatLink_type;
@synthesize factor = _factor;

+ (instancetype)flatLinkWithFactor:(CGFloat)factor {
    return [[CNFlatLink alloc] initWithFactor:factor];
}

- (instancetype)initWithFactor:(CGFloat)factor {
    self = [super init];
    if(self) _factor = factor;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFlatLink class]) _CNFlatLink_type = [CNClassType classTypeWithCls:[CNFlatLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        return [yield beginYieldWithSize:((NSUInteger)(size * _factor))];
    } yield:^CNGoR(id<CNTraversable> col) {
        return [((id<CNTraversable>)(col)) goOn:^CNGoR(id item) {
            return [yield yieldItem:item];
        }];
    }];
}

- (CNClassType*)type {
    return [CNFlatLink type];
}

+ (CNClassType*)type {
    return _CNFlatLink_type;
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

@implementation CNFlatMapLink
static CNClassType* _CNFlatMapLink_type;
@synthesize factor = _factor;
@synthesize f = _f;

+ (instancetype)flatMapLinkWithFactor:(CGFloat)factor f:(id<CNTraversable>(^)(id))f {
    return [[CNFlatMapLink alloc] initWithFactor:factor f:f];
}

- (instancetype)initWithFactor:(CGFloat)factor f:(id<CNTraversable>(^)(id))f {
    self = [super init];
    if(self) {
        _factor = factor;
        _f = [f copy];
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFlatMapLink class]) _CNFlatMapLink_type = [CNClassType classTypeWithCls:[CNFlatMapLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        return [yield beginYieldWithSize:((NSUInteger)(size * _factor))];
    } yield:^CNGoR(id item) {
        return [_f(item) goOn:^CNGoR(id i) {
            return [yield yieldItem:i];
        }];
    }];
}

- (CNClassType*)type {
    return [CNFlatMapLink type];
}

+ (CNClassType*)type {
    return _CNFlatMapLink_type;
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

