#import "objdcore.h"
#import "ODObject.h"
#import "CNChain.h"
@protocol CNIterable;
@class CNYield;
@protocol CNIterator;
@class ODClassType;

@class CNZipLink;
@class CNZip3Link;

@interface CNZipLink : NSObject<CNChainLink> {
@protected
    id<CNIterable> _a;
    id(^_f)(id, id);
}
@property (nonatomic, readonly) id<CNIterable> a;
@property (nonatomic, readonly) id(^f)(id, id);

+ (instancetype)zipLinkWithA:(id<CNIterable>)a f:(id(^)(id, id))f;
- (instancetype)initWithA:(id<CNIterable>)a f:(id(^)(id, id))f;
- (ODClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (ODClassType*)type;
@end


@interface CNZip3Link : NSObject<CNChainLink> {
@protected
    id<CNIterable> _a;
    id<CNIterable> _b;
    id(^_f)(id, id, id);
}
@property (nonatomic, readonly) id<CNIterable> a;
@property (nonatomic, readonly) id<CNIterable> b;
@property (nonatomic, readonly) id(^f)(id, id, id);

+ (instancetype)zip3LinkWithA:(id<CNIterable>)a b:(id<CNIterable>)b f:(id(^)(id, id, id))f;
- (instancetype)initWithA:(id<CNIterable>)a b:(id<CNIterable>)b f:(id(^)(id, id, id))f;
- (ODClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
+ (ODClassType*)type;
@end


