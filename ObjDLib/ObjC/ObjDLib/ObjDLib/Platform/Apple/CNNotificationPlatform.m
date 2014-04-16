#import "objd.h"
#import "CNNotificationPlatform.h"

@implementation CNNotificationCenter {
    NSNotificationCenter * _nc;
}
static CNNotificationCenter* _CNNotificationCenter_default;
static ODClassType* _CNNotificationCenter_type;

+ (id)notificationCenter {
    return [[CNNotificationCenter alloc] init];
}

- (id)init {
    self = [super init];
    if(self) {
        _nc = [NSNotificationCenter defaultCenter];
    }
    return self;
}

+ (void)initialize {
    [super initialize];
    _CNNotificationCenter_type = [ODClassType classTypeWithCls:[CNNotificationCenter class]];
    _CNNotificationCenter_default = [CNNotificationCenter notificationCenter];
}

- (CNNotificationObserver*)addObserverName:(NSString*)name sender:(id)sender block:(void(^)(id, id))block {
    return [CNNotificationObserver observerWithObserverHandle:[_nc addObserverForName:name object:sender queue:nil usingBlock:^(NSNotification *note) {
        block(note.object, [note.userInfo valueForKey:@"data"]);
    }]];
}

- (void)postName:(NSString*)name sender:(id)sender data:(id)data {
    [_nc postNotificationName:name object:sender userInfo:
            data == nil ? [NSDictionary dictionary]
                    : [NSDictionary dictionaryWithObject:data forKey:@"data"]];
}

- (ODClassType*)type {
    return [CNNotificationCenter type];
}

+ (CNNotificationCenter*)instance {
    return _CNNotificationCenter_default;
}

+ (ODClassType*)type {
    return _CNNotificationCenter_type;
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


@implementation CNNotificationObserver {
    id _observerHandle;
}
static ODClassType* _CNNotificationObserver_type;

- (instancetype)initWithObserverHandle:(id)observerHandle {
    self = [super init];
    if (self) {
        _observerHandle = observerHandle;
    }

    return self;
}

+ (instancetype)observerWithObserverHandle:(id)observerHandle {
    return [[self alloc] initWithObserverHandle:observerHandle];
}


+ (void)initialize {
    [super initialize];
    _CNNotificationObserver_type = [ODClassType classTypeWithCls:[CNNotificationObserver class]];
}

- (void)dealloc {
    [self detach];
}


- (void)detach {
    [[NSNotificationCenter defaultCenter] removeObserver:_observerHandle];
}

- (ODClassType*)type {
    return [CNNotificationObserver type];
}

+ (ODClassType*)type {
    return _CNNotificationObserver_type;
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


