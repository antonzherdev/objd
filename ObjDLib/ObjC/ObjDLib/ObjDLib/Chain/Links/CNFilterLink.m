#import "objd.h"
#import "CNFilterLink.h"

#import "CNType.h"
@implementation CNFilterLink
static CNClassType* _CNFilterLink_type;
@synthesize predicate = _predicate;
@synthesize factor = _factor;

+ (instancetype)filterLinkWithPredicate:(BOOL(^)(id))predicate factor:(CGFloat)factor {
    return [[CNFilterLink alloc] initWithPredicate:predicate factor:factor];
}

- (instancetype)initWithPredicate:(BOOL(^)(id))predicate factor:(CGFloat)factor {
    self = [super init];
    if(self) {
        _predicate = [predicate copy];
        _factor = factor;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFilterLink class]) _CNFilterLink_type = [CNClassType classTypeWithCls:[CNFilterLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        return [yield beginYieldWithSize:((NSUInteger)(size * _factor))];
    } yield:^CNGoR(id item) {
        if(_predicate(item)) return [yield yieldItem:item];
        else return CNGo_Continue;
    }];
}

- (CNClassType*)type {
    return [CNFilterLink type];
}

+ (CNClassType*)type {
    return _CNFilterLink_type;
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

@implementation CNTopLink
static CNClassType* _CNTopLink_type;
@synthesize number = _number;

+ (instancetype)topLinkWithNumber:(NSUInteger)number {
    return [[CNTopLink alloc] initWithNumber:number];
}

- (instancetype)initWithNumber:(NSUInteger)number {
    self = [super init];
    if(self) _number = number;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNTopLink class]) _CNTopLink_type = [CNClassType classTypeWithCls:[CNTopLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    __block NSUInteger n = 0;
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        return [yield beginYieldWithSize:uintMaxB(size, _number)];
    } yield:^CNGoR(id item) {
        if(n < _number) {
            if([yield yieldItem:item] == CNGo_Break) {
                return CNGo_Break;
            } else {
                n++;
                if(n < _number) return CNGo_Continue;
                else return CNGo_Break;
            }
        } else {
            return CNGo_Break;
        }
    }];
}

- (CNClassType*)type {
    return [CNTopLink type];
}

+ (CNClassType*)type {
    return _CNTopLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"number=%lu", (unsigned long)self.number];
    [description appendString:@">"];
    return description;
}

@end

