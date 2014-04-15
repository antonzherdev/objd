#import "objd.h"
#import "CNNotification.h"

#import "CNNotificationPlatform.h"
#import "ODType.h"
@implementation CNNotificationHandle
static ODClassType* _CNNotificationHandle_type;
@synthesize name = _name;

+ (instancetype)notificationHandleWithName:(NSString*)name {
    return [[CNNotificationHandle alloc] initWithName:name];
}

- (instancetype)initWithName:(NSString*)name {
    self = [super init];
    if(self) _name = name;
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNNotificationHandle class]) _CNNotificationHandle_type = [ODClassType classTypeWithCls:[CNNotificationHandle class]];
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

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"name=%@", self.name];
    [description appendString:@">"];
    return description;
}

@end


