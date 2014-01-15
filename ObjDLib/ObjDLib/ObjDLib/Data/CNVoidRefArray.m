#import "CNVoidRefArray.h"

#import "CNData.h"
CNVoidRefArray cnVoidRefArrayApplyLength(NSUInteger length) {
    return (CNVoidRefArray){length, malloc(length)};
}
CNVoidRefArray cnVoidRefArrayApplyTpCount(ODPType* tp, NSUInteger count) {
    return cnVoidRefArrayApplyLength(tp.size * count);
}
ODPType* cnVoidRefArrayType() {
    static ODPType* _ret = nil;
    if(_ret == nil) _ret = [ODPType typeWithCls:[CNVoidRefArrayWrap class] name:@"CNVoidRefArray" size:sizeof(CNVoidRefArray) wrap:^id(void* data, NSUInteger i) {
        return wrap(CNVoidRefArray, ((CNVoidRefArray*)(data))[i]);
    }];
    return _ret;
}
@implementation CNVoidRefArrayWrap{
    CNVoidRefArray _value;
}
@synthesize value = _value;

+ (id)wrapWithValue:(CNVoidRefArray)value {
    return [[CNVoidRefArrayWrap alloc] initWithValue:value];
}

- (id)initWithValue:(CNVoidRefArray)value {
    self = [super init];
    if(self) _value = value;
    return self;
}

- (NSString*)description {
    return CNVoidRefArrayDescription(_value);
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNVoidRefArrayWrap* o = ((CNVoidRefArrayWrap*)(other));
    return CNVoidRefArrayEq(_value, o.value);
}

- (NSUInteger)hash {
    return CNVoidRefArrayHash(_value);
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

@end



