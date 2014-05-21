#import "objdcore.h"
#import "CNYield.h"
#import "CNCollection.h"
@class CNMHashMap;
@class CNClassType;
@class CNMHashSet;

@class CNImGroupByLink;
@class CNMGroupByLink;
@class CNDistinctLink;

@interface CNImGroupByLink : CNChainLink_impl {
@protected
    id(^_by)(id);
    id(^_start)();
    id(^_fold)(id, id);
    CGFloat _factor;
}
@property (nonatomic, readonly) id(^by)(id);
@property (nonatomic, readonly) id(^start)();
@property (nonatomic, readonly) id(^fold)(id, id);
@property (nonatomic, readonly) CGFloat factor;

+ (instancetype)imGroupByLinkWithBy:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold factor:(CGFloat)factor;
- (instancetype)initWithBy:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold factor:(CGFloat)factor;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


@interface CNMGroupByLink : CNChainLink_impl {
@protected
    id(^_by)(id);
    id(^_start)();
    void(^_append)(id, id);
    id(^_finish)(id);
    CGFloat _factor;
}
@property (nonatomic, readonly) id(^by)(id);
@property (nonatomic, readonly) id(^start)();
@property (nonatomic, readonly) void(^append)(id, id);
@property (nonatomic, readonly) id(^finish)(id);
@property (nonatomic, readonly) CGFloat factor;

+ (instancetype)groupByLinkWithBy:(id(^)(id))by start:(id(^)())start append:(void(^)(id, id))append finish:(id(^)(id))finish factor:(CGFloat)factor;
- (instancetype)initWithBy:(id(^)(id))by start:(id(^)())start append:(void(^)(id, id))append finish:(id(^)(id))finish factor:(CGFloat)factor;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


@interface CNDistinctLink : CNChainLink_impl {
@protected
    CGFloat _factor;
}
@property (nonatomic, readonly) CGFloat factor;

+ (instancetype)distinctLinkWithFactor:(CGFloat)factor;
- (instancetype)initWithFactor:(CGFloat)factor;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (CNClassType*)type;
@end


