#import "objdcore.h"
#import "ODObject.h"
@class ODClassType;

@class CNLazy;
@class CNCache;
@class CNWeak;

@interface CNLazy : NSObject
@property (nonatomic, readonly) id(^f)();

+ (id)lazyWithF:(id(^)())f;
- (id)initWithF:(id(^)())f;
- (ODClassType*)type;
- (BOOL)isCalculated;
- (id)get;
+ (ODClassType*)type;
@end


@interface CNCache : NSObject
@property (nonatomic, readonly) id(^f)(id);

+ (id)cacheWithF:(id(^)(id))f;
- (id)initWithF:(id(^)(id))f;
- (ODClassType*)type;
- (id)applyX:(id)x;
+ (ODClassType*)type;
@end


@interface CNWeak : NSObject
@property (nonatomic, readonly, weak) id get;

+ (id)weakWithGet:(id)get;
- (id)initWithGet:(id)get;
- (ODClassType*)type;
+ (ODClassType*)type;
@end


