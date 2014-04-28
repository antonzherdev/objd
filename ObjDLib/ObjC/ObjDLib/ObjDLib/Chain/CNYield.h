#import "objdcore.h"
#import "ODObject.h"
@protocol CNTraversable;
@class CNArray;
@protocol CNIterable;
@class ODClassType;

@class CNYield;

@interface CNYield : NSObject {
@protected
    int(^_begin)(NSUInteger);
    int(^_yield)(id);
    int(^_end)(int);
    int(^_all)(id<CNTraversable>);
}
@property (nonatomic, readonly) int(^begin)(NSUInteger);
@property (nonatomic, readonly) int(^yield)(id);
@property (nonatomic, readonly) int(^end)(int);
@property (nonatomic, readonly) int(^all)(id<CNTraversable>);

+ (instancetype)yieldWithBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
- (instancetype)initWithBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
- (ODClassType*)type;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)makeBegin:(int(^)(NSUInteger))begin;
+ (CNYield*)makeYield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)makeYield:(int(^)(id))yield end:(int(^)(int))end;
+ (CNYield*)makeYield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)makeYield:(int(^)(id))yield;
+ (CNYield*)makeEnd:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)makeEnd:(int(^)(int))end;
+ (CNYield*)makeAll:(int(^)(id<CNTraversable>))all;
+ (CNYield*)make;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin end:(int(^)(int))end;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base begin:(int(^)(NSUInteger))begin;
+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield end:(int(^)(int))end;
+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base yield:(int(^)(id))yield;
+ (CNYield*)decorateBase:(CNYield*)base end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base end:(int(^)(int))end;
+ (CNYield*)decorateBase:(CNYield*)base all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)decorateBase:(CNYield*)base;
- (int)beginYieldWithSize:(NSUInteger)size;
- (int)yieldItem:(id)item;
- (int)endYieldWithResult:(int)result;
- (int)yieldAllItems:(id<CNTraversable>)items;
- (int)stdYieldAllItems:(id<CNTraversable>)items;
+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield end:(int(^)(int))end;
+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin yield:(int(^)(id))yield;
+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin end:(int(^)(int))end;
+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)applyBegin:(int(^)(NSUInteger))begin;
+ (CNYield*)applyYield:(int(^)(id))yield end:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)applyYield:(int(^)(id))yield end:(int(^)(int))end;
+ (CNYield*)applyYield:(int(^)(id))yield all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)applyYield:(int(^)(id))yield;
+ (CNYield*)applyEnd:(int(^)(int))end all:(int(^)(id<CNTraversable>))all;
+ (CNYield*)applyEnd:(int(^)(int))end;
+ (CNYield*)applyAll:(int(^)(id<CNTraversable>))all;
+ (CNYield*)apply;
+ (ODClassType*)type;
@end


