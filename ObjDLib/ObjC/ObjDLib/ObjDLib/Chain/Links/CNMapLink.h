#import "objdcore.h"
#import "CNObject.h"
#import "CNChain.h"
@class CNYield;
@class CNClassType;

@class CNMapLink;
@class CNMapOptLink;

@interface CNMapLink : NSObject<CNChainLink> {
@protected
    id(^_f)(id);
}
@property (nonatomic, readonly) id(^f)(id);

+ (instancetype)mapLinkWithF:(id(^)(id))f;
- (instancetype)initWithF:(id(^)(id))f;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


@interface CNMapOptLink : NSObject<CNChainLink> {
@protected
    id(^_f)(id);
}
@property (nonatomic, readonly) id(^f)(id);

+ (instancetype)mapOptLinkWithF:(id(^)(id))f;
- (instancetype)initWithF:(id(^)(id))f;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


