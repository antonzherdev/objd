#import "CNFilterLink.h"

@implementation CNFilterLink
static CNClassType* _CNFilterLink_type;
@synthesize factor = _factor;
@synthesize predicate = _predicate;

+ (instancetype)filterLinkWithFactor:(CGFloat)factor predicate:(BOOL(^)(id))predicate {
    return [[CNFilterLink alloc] initWithFactor:factor predicate:predicate];
}

- (instancetype)initWithFactor:(CGFloat)factor predicate:(BOOL(^)(id))predicate {
    self = [super init];
    if(self) {
        _factor = factor;
        _predicate = [predicate copy];
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

- (NSString*)description {
    return [NSString stringWithFormat:@"FilterLink(%f)", _factor];
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

- (NSString*)description {
    return [NSString stringWithFormat:@"TopLink(%lu)", (unsigned long)_number];
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

@end

