#import "objd.h"
#import "CNSourceLink.h"

#import "CNType.h"
@implementation CNSourceLink
static CNClassType* _CNSourceLink_type;
@synthesize collection = _collection;

+ (instancetype)sourceLinkWithCollection:(id<CNTraversable>)collection {
    return [[CNSourceLink alloc] initWithCollection:collection];
}

- (instancetype)initWithCollection:(id<CNTraversable>)collection {
    self = [super init];
    if(self) _collection = collection;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNSourceLink class]) _CNSourceLink_type = [CNClassType classTypeWithCls:[CNSourceLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield makeBegin:({
        id<CNIterable> c = [CNObject asKindOfProtocol:@protocol(CNIterable) object:_collection];
        ((c != nil) ? ^CNGoR(NSUInteger size) {
            return [yield beginYieldWithSize:[c count]];
        } : nil);
    }) end:^CNGoR(CNGoR result) {
        if(result == CNGo_Break) return [yield endYieldWithResult:result];
        else return [yield yieldAllItems:_collection];
    }];
}

- (CNClassType*)type {
    return [CNSourceLink type];
}

+ (CNClassType*)type {
    return _CNSourceLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"collection=%@", self.collection];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNAppendLink
static CNClassType* _CNAppendLink_type;
@synthesize collection = _collection;

+ (instancetype)appendLinkWithCollection:(id<CNTraversable>)collection {
    return [[CNAppendLink alloc] initWithCollection:collection];
}

- (instancetype)initWithCollection:(id<CNTraversable>)collection {
    self = [super init];
    if(self) _collection = collection;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNAppendLink class]) _CNAppendLink_type = [CNClassType classTypeWithCls:[CNAppendLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:({
        id<CNIterable> c = [CNObject asKindOfProtocol:@protocol(CNIterable) object:_collection];
        ((c != nil) ? ^CNGoR(NSUInteger size) {
            return [yield beginYieldWithSize:size + [c count]];
        } : nil);
    }) end:^CNGoR(CNGoR result) {
        if(result == CNGo_Continue) return [yield endYieldWithResult:[_collection goOn:^CNGoR(id item) {
            return [yield yieldItem:item];
        }]];
        else return [yield endYieldWithResult:result];
    }];
}

- (CNClassType*)type {
    return [CNAppendLink type];
}

+ (CNClassType*)type {
    return _CNAppendLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"collection=%@", self.collection];
    [description appendString:@">"];
    return description;
}

@end

@implementation CNPrependLink
static CNClassType* _CNPrependLink_type;
@synthesize collection = _collection;

+ (instancetype)prependLinkWithCollection:(id<CNTraversable>)collection {
    return [[CNPrependLink alloc] initWithCollection:collection];
}

- (instancetype)initWithCollection:(id<CNTraversable>)collection {
    self = [super init];
    if(self) _collection = collection;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNPrependLink class]) _CNPrependLink_type = [CNClassType classTypeWithCls:[CNPrependLink class]];
}

- (CNYield*)buildYield:(CNYield*)yield {
    return [CNYield decorateBase:yield begin:^CNGoR(NSUInteger size) {
        CNGoR r = [yield beginYieldWithSize:size + ({
            id __tmprp1_0rp0b;
            {
                id<CNIterable> _ = [CNObject asKindOfProtocol:@protocol(CNIterable) object:_collection];
                if(_ != nil) __tmprp1_0rp0b = numui([_ count]);
                else __tmprp1_0rp0b = nil;
            }
            ((__tmprp1_0rp0b != nil) ? unumui(__tmprp1_0rp0b) : 0);
        })];
        if(r == CNGo_Break) return CNGo_Break;
        else return [_collection goOn:^CNGoR(id item) {
            return [yield yieldItem:item];
        }];
    }];
}

- (CNClassType*)type {
    return [CNPrependLink type];
}

+ (CNClassType*)type {
    return _CNPrependLink_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"collection=%@", self.collection];
    [description appendString:@">"];
    return description;
}

@end

