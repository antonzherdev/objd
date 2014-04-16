#import <Foundation/Foundation.h>

#define cnYieldContinue 0
#define cnYieldBreak 1
typedef NSInteger CNYieldResult;

typedef CNYieldResult (^cnYield)(id item);
typedef CNYieldResult (^cnYieldBegin)(NSUInteger size);
typedef CNYieldResult (^cnYieldEnd)(CNYieldResult result);
typedef CNYieldResult (^cnYieldAll)(id collection);

@interface CNYield : NSObject
- (id)initWithBegin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end all:(cnYieldAll)all;

+ (CNYield*)yieldWithBegin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end all:(cnYieldAll)all;
+ (CNYield*)applyBegin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end;
+ (CNYield*)applyYield:(cnYield)yield end:(cnYieldEnd)end;
+ (CNYield*)applyBegin:(cnYieldBegin)begin yield:(cnYield)yield;
+ (CNYield*)decorateBase:(CNYield *)base begin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end all:(cnYieldAll)all;
+ (CNYield*)decorateBase:(CNYield *)base begin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end;
+ (CNYield*)decorateBase:(CNYield *)base yield:(cnYield)yield end:(cnYieldEnd)end;
+ (CNYield*)decorateBase:(CNYield *)base begin:(cnYieldBegin)begin yield:(cnYield)yield;

+ (CNYield*)decorateBase:(CNYield *)base yield:(cnYield)yield;

+ (CNYieldResult) yieldAll:(id)collection byItemsTo:(CNYield *) yield;

- (CNYieldResult) beginYieldWithSize:(NSUInteger)size;

- (CNYieldResult) yieldItem:(id)item;
- (CNYieldResult) endYieldWithResult:(CNYieldResult)result;
- (CNYieldResult) yieldAll:(id)collection;
@end