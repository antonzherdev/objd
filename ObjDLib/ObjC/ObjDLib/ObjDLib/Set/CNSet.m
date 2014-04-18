#import "objd.h"
#import "CNSet.h"

#import "CNDispatchQueue.h"
#import "CNChain.h"
#import "CNPlat.h"
#import "ODType.h"
@implementation CNHashSetBuilder
static ODClassType* _CNHashSetBuilder_type;
@synthesize set = _set;

+ (instancetype)hashSetBuilder {
    return [[CNHashSetBuilder alloc] init];
}

- (instancetype)init {
    self = [super init];
    if(self) _set = [CNMHashSet hashSet];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNHashSetBuilder class]) _CNHashSetBuilder_type = [ODClassType classTypeWithCls:[CNHashSetBuilder class]];
}

- (void)appendItem:(id)item {
    [_set appendItem:item];
}

- (CNImHashSet*)build {
    return [_set im];
}

- (void)appendAllItems:(id<CNTraversable>)items {
    [items forEach:^void(id _) {
        [self appendItem:_];
    }];
}

- (ODClassType*)type {
    return [CNHashSetBuilder type];
}

+ (ODClassType*)type {
    return _CNHashSetBuilder_type;
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


