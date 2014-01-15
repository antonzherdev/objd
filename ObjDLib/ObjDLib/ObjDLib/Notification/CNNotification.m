#import "objd.h"
#import "CNNotification.h"

#import "CNNotificationPlatform.h"
#import "ODType.h"
@implementation CNNotificationHandle{
    NSString* _name;
}
static ODClassType* _CNNotificationHandle_type;
@synthesize name = _name;

+ (id)notificationHandleWithName:(NSString*)name {
    return [[CNNotificationHandle alloc] initWithName:name];
}

- (id)initWithName:(NSString*)name {
    self = [super init];
    if(self) _name = name;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNNotificationHandle_type = [ODClassType classTypeWithCls:[CNNotificationHandle class]];
}

- (void)postSender:(id)sender {
    [CNNotificationCenter.instance postName:_name sender:sender data:nil];
}

- (void)postSender:(id)sender data:(id)data {
    [CNNotificationCenter.instance postName:_name sender:sender data:data];
}

- (CNNotificationObserver*)observeBy:(void(^)(id, id))by {
    return [CNNotificationCenter.instance addObserverName:_name sender:nil block:^void(id sender, id data) {
        by(sender, data);
    }];
}

- (CNNotificationObserver*)observeSender:(id)sender by:(void(^)(id))by {
    return [CNNotificationCenter.instance addObserverName:_name sender:sender block:^void(id _, id data) {
        by(data);
    }];
}

- (ODClassType*)type {
    return [CNNotificationHandle type];
}

+ (ODClassType*)type {
    return _CNNotificationHandle_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (BOOL)isEqual:(id)other {
    if(self == other) return YES;
    if(!(other) || !([[self class] isEqual:[other class]])) return NO;
    CNNotificationHandle* o = ((CNNotificationHandle*)(other));
    return [self.name isEqual:o.name];
}

- (NSUInteger)hash {
    NSUInteger hash = 0;
    hash = hash * 31 + [self.name hash];
    return hash;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"name=%@", self.name];
    [description appendString:@">"];
    return description;
}

@end


