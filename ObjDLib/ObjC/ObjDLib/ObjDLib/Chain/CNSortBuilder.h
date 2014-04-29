#import "objdcore.h"
#import "CNObject.h"
@class CNChain;
@class CNMArray;
@protocol CNIterator;
@class CNClassType;

@class CNSortBuilder;

@interface CNSortBuilder : NSObject {
@protected
    CNChain* _chain;
    CNMArray* _functions;
}
@property (nonatomic, readonly) CNChain* chain;

+ (instancetype)sortBuilderWithChain:(CNChain*)chain;
- (instancetype)initWithChain:(CNChain*)chain;
- (CNClassType*)type;
- (CNSortBuilder*)ascBy:(id(^)(id))by;
- (CNSortBuilder*)descBy:(id(^)(id))by;
- (CNSortBuilder*)andF:(NSInteger(^)(id, id))f;
- (CNChain*)endSort;
+ (CNClassType*)type;
@end


