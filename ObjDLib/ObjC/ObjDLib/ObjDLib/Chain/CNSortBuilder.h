#import "objdcore.h"
#import "ODObject.h"
@class CNChain;
@class CNMArray;
@protocol CNIterator;
@class ODClassType;

@class CNSortBuilder;

@interface CNSortBuilder : NSObject {
@protected
    CNChain* _chain;
    CNMArray* _functions;
}
@property (nonatomic, readonly) CNChain* chain;

+ (instancetype)sortBuilderWithChain:(CNChain*)chain;
- (instancetype)initWithChain:(CNChain*)chain;
- (ODClassType*)type;
- (CNSortBuilder*)ascBy:(id(^)(id))by;
- (CNSortBuilder*)descBy:(id(^)(id))by;
- (CNSortBuilder*)andF:(NSInteger(^)(id, id))f;
- (CNChain*)endSort;
+ (ODClassType*)type;
@end


