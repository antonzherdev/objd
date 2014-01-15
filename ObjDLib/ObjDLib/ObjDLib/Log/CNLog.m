#import "objd.h"
#import "CNLog.h"

#import "ODType.h"
@implementation CNLog
static ODClassType* _CNLog_type;

+ (id)log {
    return [[CNLog alloc] init];
}

- (id)init {
    self = [super init];
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNLog_type = [ODClassType classTypeWithCls:[CNLog class]];
}

+ (void)applyText:(NSString*)text {
    NSLog(@"%@", text);
}

- (ODClassType*)type {
    return [CNLog type];
}

+ (ODClassType*)type {
    return _CNLog_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    return YES;
}

- (NSUInteger)hash {
    return 0;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendString:@">"];
    return description;
}

@end


