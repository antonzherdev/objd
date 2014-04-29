#import "objdcore.h"
#import "CNYield.h"
@class CNClassType;

@class CNMapLink;
@class CNMapOptLink;

@interface CNMapLink : CNChainLink_impl {
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


@interface CNMapOptLink : CNChainLink_impl {
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


