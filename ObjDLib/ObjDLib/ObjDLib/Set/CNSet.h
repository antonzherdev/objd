#import "objdcore.h"
#import "CNCollection.h"
@class ODClassType;
@class CNDispatchQueue;
@class CNChain;

@class CNHashSetBuilder;
@protocol CNSet;
@protocol CNImSet;
@protocol CNMSet;

@protocol CNSet<CNIterable>
@end


@protocol CNImSet<CNSet, CNImIterable>
- (id<CNMSet>)mCopy;
@end


@protocol CNMSet<CNSet, CNMIterable>
- (id<CNImSet>)im;
- (id<CNImSet>)imCopy;
@end


@interface CNHashSetBuilder : NSObject<CNBuilder> {
@protected
    NSMutableSet* _set;
}
@property (nonatomic, readonly) NSMutableSet* set;

+ (instancetype)hashSetBuilder;
- (instancetype)init;
- (ODClassType*)type;
- (void)appendItem:(id)item;
- (NSSet*)build;
+ (ODClassType*)type;
@end


