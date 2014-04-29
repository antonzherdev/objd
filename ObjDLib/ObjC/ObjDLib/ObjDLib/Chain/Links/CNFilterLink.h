#import "objdcore.h"
#import "CNObject.h"
#import "CNChain.h"
@class CNYield;
@class CNClassType;

@class CNFilterLink;

@interface CNFilterLink : NSObject<CNChainLink> {
@protected
    BOOL(^_predicate)(id);
    float _selectivity;
}
@property (nonatomic, readonly) BOOL(^predicate)(id);
@property (nonatomic, readonly) float selectivity;

+ (instancetype)filterLinkWithPredicate:(BOOL(^)(id))predicate selectivity:(float)selectivity;
- (instancetype)initWithPredicate:(BOOL(^)(id))predicate selectivity:(float)selectivity;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end

