#import "objdcore.h"
#import "CNYield.h"
#import "CNCollection.h"
@class CNClassType;

@class CNMapLink;
@class CNMapOptLink;
@class CNFlatLink;
@class CNFlatMapLink;

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


@interface CNFlatLink : CNChainLink_impl {
@protected
    CGFloat _factor;
}
@property (nonatomic, readonly) CGFloat factor;

+ (instancetype)flatLinkWithFactor:(CGFloat)factor;
- (instancetype)initWithFactor:(CGFloat)factor;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


@interface CNFlatMapLink : CNChainLink_impl {
@protected
    id<CNTraversable>(^_f)(id);
    CGFloat _factor;
}
@property (nonatomic, readonly) id<CNTraversable>(^f)(id);
@property (nonatomic, readonly) CGFloat factor;

+ (instancetype)flatMapLinkWithF:(id<CNTraversable>(^)(id))f factor:(CGFloat)factor;
- (instancetype)initWithF:(id<CNTraversable>(^)(id))f factor:(CGFloat)factor;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


