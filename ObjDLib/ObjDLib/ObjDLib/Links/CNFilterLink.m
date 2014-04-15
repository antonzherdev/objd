#import "objd.h"
#import "CNFilterLink.h"

#import "CNYield.h"
#import "ODType.h"
@implementation CNFilterLink
static ODClassType* _CNFilterLink_type;
@synthesize predicate = _predicate;
@synthesize selectivity = _selectivity;

+ (instancetype)filterLinkWithPredicate:(BOOL(^)(id))predicate selectivity:(float)selectivity {
    return [[CNFilterLink alloc] initWithPredicate:predicate selectivity:selectivity];
}

- (instancetype)initWithPredicate:(BOOL(^)(id))predicate selectivity:(float)selectivity {
    self = [super init];
    if(self) {
        _predicate = [predicate copy];
        _selectivity = selectivity;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNFilterLink class]) _CNFilterLink_type = [ODClassType classTypeWithCls:[CNFilterLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:^NSInteger(NSUInteger size) {
        return [yield beginYieldWithSize:((NSUInteger)(size * _selectivity))];
    } yield:^NSInteger(id<CNTraversable> item) {
        if(_predicate(item)) return [yield yieldItem:item];
        else return 0;
    }];
}

- (ODClassType*)type {
    return [CNFilterLink type];
}

+ (ODClassType*)type {
    return _CNFilterLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"selectivity=%f", self.selectivity];
    [description appendString:@">"];
    return description;
}

@end


