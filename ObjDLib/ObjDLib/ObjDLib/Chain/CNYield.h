#import <Foundation/Foundation.h>

typedef enum {
    cnYieldContinue,
    cnYieldBreak,
} CNYieldResult;

typedef CNYieldResult (^cnYield)(id item);
typedef CNYieldResult (^cnYieldBegin)(NSUInteger size);
typedef CNYieldResult (^cnYieldEnd)(CNYieldResult result);
typedef CNYieldResult (^cnYieldAll)(id collection);

@interface CNYield : NSObject
- (id)initWithBegin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end all:(cnYieldAll)all;

+ (CNYield*)yieldWithBegin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end all:(cnYieldAll)all;
+ (CNYield*)decorateYield:(CNYield*)base begin:(cnYieldBegin)begin yield:(cnYield)yield end:(cnYieldEnd)end all:(cnYieldAll)all;

+ (CNYieldResult) yieldAll:(id)collection byItemsTo:(CNYield *) yield;

- (CNYieldResult) beginYieldWithSize:(NSUInteger)size;
- (CNYieldResult) yieldItem:(id)item;
- (CNYieldResult) endYieldWithResult:(CNYieldResult)result;
- (CNYieldResult) yieldAll:(id)collection;
@end
