#import "objdcore.h"
#import "CNObject.h"
#import "CNCollection.h"
@class CNClassType;
@class CNArray;

@class CNChainLink_impl;
@class CNYield;
@protocol CNChainLink;

@protocol CNChainLink<NSObject>
- (CNYield*)buildYield:(CNYield*)yield;
- (NSString*)description;
@end


@interface CNChainLink_impl : NSObject<CNChainLink>
@end


@interface CNYield : NSObject {
@protected
    CNGoR(^_begin)(NSUInteger);
    CNGoR(^_yield)(id);
    CNGoR(^_end)(CNGoR);
    CNGoR(^_all)(CNYield*, id<CNTraversable>);
}
@property (nonatomic, readonly) CNGoR(^begin)(NSUInteger);
@property (nonatomic, readonly) CNGoR(^yield)(id);
@property (nonatomic, readonly) CNGoR(^end)(CNGoR);
@property (nonatomic, readonly) CNGoR(^all)(CNYield*, id<CNTraversable>);

+ (instancetype)yieldWithBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
- (instancetype)initWithBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
- (CNClassType*)type;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)makeBegin:(CNGoR(^)(NSUInteger))begin;
+ (CNYield*)makeYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)makeYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)makeYield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)makeYield:(CNGoR(^)(id))yield;
+ (CNYield*)makeEnd:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)makeEnd:(CNGoR(^)(CNGoR))end;
+ (CNYield*)makeAll:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)make;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(CNGoR(^)(NSUInteger))begin;
+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base yield:(CNGoR(^)(id))yield;
+ (CNYield*)decorateBase:(CNYield*)base end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)decorateBase:(CNYield*)base all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base;
- (CNGoR)beginYieldWithSize:(NSUInteger)size;
- (CNGoR)yieldItem:(id)item;
- (CNGoR)endYieldWithResult:(CNGoR)result;
- (CNGoR)yieldAllItems:(id<CNTraversable>)items;
- (CNGoR)stdYieldAllItems:(id<CNTraversable>)items;
+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin yield:(CNGoR(^)(id))yield;
+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)applyBegin:(CNGoR(^)(NSUInteger))begin;
+ (CNYield*)applyYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)applyYield:(CNGoR(^)(id))yield end:(CNGoR(^)(CNGoR))end;
+ (CNYield*)applyYield:(CNGoR(^)(id))yield all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)applyYield:(CNGoR(^)(id))yield;
+ (CNYield*)applyEnd:(CNGoR(^)(CNGoR))end all:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)applyEnd:(CNGoR(^)(CNGoR))end;
+ (CNYield*)applyAll:(CNGoR(^)(CNYield*, id<CNTraversable>))all;
+ (CNYield*)apply;
- (NSString*)description;
+ (CNClassType*)type;
@end


