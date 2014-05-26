#import "objdcore.h"
#import "CNYield.h"
#import "CNCollection.h"
@class CNClassType;

@class CNSourceLink;
@class CNAppendLink;
@class CNPrependLink;

@interface CNSourceLink : CNChainLink_impl {
@protected
    id<CNTraversable> _collection;
}
@property (nonatomic, readonly) id<CNTraversable> collection;

+ (instancetype)sourceLinkWithCollection:(id<CNTraversable>)collection;
- (instancetype)initWithCollection:(id<CNTraversable>)collection;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNAppendLink : CNChainLink_impl {
@protected
    id<CNTraversable> _collection;
}
@property (nonatomic, readonly) id<CNTraversable> collection;

+ (instancetype)appendLinkWithCollection:(id<CNTraversable>)collection;
- (instancetype)initWithCollection:(id<CNTraversable>)collection;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


@interface CNPrependLink : CNChainLink_impl {
@protected
    id<CNTraversable> _collection;
}
@property (nonatomic, readonly) id<CNTraversable> collection;

+ (instancetype)prependLinkWithCollection:(id<CNTraversable>)collection;
- (instancetype)initWithCollection:(id<CNTraversable>)collection;
- (CNClassType*)type;
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
+ (CNClassType*)type;
@end


