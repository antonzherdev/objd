#import "objd.h"
#import "CNYield.h"

@class CNFilterLink;
@class CNTopLink;

@interface CNFilterLink : CNChainLink_impl {
@public
    CGFloat _factor;
    BOOL(^_predicate)(id);
}
@property (nonatomic, readonly) CGFloat factor;
@property (nonatomic, readonly) BOOL(^predicate)(id);

+ (instancetype)filterLinkWithFactor:(CGFloat)factor predicate:(BOOL(^)(id))predicate;
- (instancetype)initWithFactor:(CGFloat)factor predicate:(BOOL(^)(id))predicate;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNTopLink : CNChainLink_impl {
@public
    NSUInteger _number;
}
@property (nonatomic, readonly) NSUInteger number;

+ (instancetype)topLinkWithNumber:(NSUInteger)number;
- (instancetype)initWithNumber:(NSUInteger)number;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


