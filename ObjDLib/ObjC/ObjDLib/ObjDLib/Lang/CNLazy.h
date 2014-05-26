#import "objdcore.h"
#import "CNObject.h"
@class CNClassType;

@class CNLazy;
@class CNCache;
@class CNWeak;

@interface CNLazy : NSObject {
@protected
    id(^_f)();
    id __value;
    BOOL __calculated;
}
@property (nonatomic, readonly) id(^f)();

+ (instancetype)lazyWithF:(id(^)())f;
- (instancetype)initWithF:(id(^)())f;
- (CNClassType*)type;
- (BOOL)isCalculated;
- (id)get;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNCache : NSObject {
@protected
    id(^_f)(id);
    id __lastX;
    id __lastF;
}
@property (nonatomic, readonly) id(^f)(id);

+ (instancetype)cacheWithF:(id(^)(id))f;
- (instancetype)initWithF:(id(^)(id))f;
- (CNClassType*)type;
- (id)applyX:(id)x;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNWeak : NSObject {
@protected
    __weak id _value;
}
@property (nonatomic, readonly, weak) id value;

+ (instancetype)weakWithValue:(id)value;
- (instancetype)initWithValue:(id)value;
- (CNClassType*)type;
- (BOOL)isEmpty;
- (NSString*)description;
+ (CNClassType*)type;
@end


