#import "objdcore.h"
#import "CNYield.h"
#import "CNCollection.h"
@class CNClassType;

@class CNFilterLink;
@class CNTopLink;

@interface CNFilterLink : CNChainLink_impl {
@protected
    BOOL(^_predicate)(id);
    CGFloat _factor;
}
@property (nonatomic, readonly) BOOL(^predicate)(id);
@property (nonatomic, readonly) CGFloat factor;

+ (instancetype)filterLinkWithPredicate:(BOOL(^)(id))predicate factor:(CGFloat)factor;
- (instancetype)initWithPredicate:(BOOL(^)(id))predicate factor:(CGFloat)factor;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


@interface CNTopLink : CNChainLink_impl {
@protected
    NSUInteger _number;
}
@property (nonatomic, readonly) NSUInteger number;

+ (instancetype)topLinkWithNumber:(NSUInteger)number;
- (instancetype)initWithNumber:(NSUInteger)number;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


