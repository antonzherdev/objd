#import "objdcore.h"
#import "CNChain.h"
@class CNYield;
@class ODClassType;

@class CNMapLink;
@class CNMapOptLink;

@interface CNMapLink : NSObject<CNChainLink> {
@protected
    id(^_f)(id);
}
@property (nonatomic, readonly) id(^f)(id);

+ (instancetype)mapLinkWithF:(id(^)(id))f;
- (instancetype)initWithF:(id(^)(id))f;
- (ODClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (ODClassType*)type;
@end


@interface CNMapOptLink : NSObject<CNChainLink> {
@protected
    id(^_f)(id);
}
@property (nonatomic, readonly) id(^f)(id);

+ (instancetype)mapOptLinkWithF:(id(^)(id))f;
- (instancetype)initWithF:(id(^)(id))f;
- (ODClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (ODClassType*)type;
@end


