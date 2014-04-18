#import "objdcore.h"
#import "CNCollection.h"
@class CNDispatchQueue;
@class CNChain;
@class CNMHashSet;
@class CNImHashSet;
@class ODClassType;

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
    CNMHashSet* _set;
}
@property (nonatomic, readonly) CNMHashSet* set;

+ (instancetype)hashSetBuilder;
- (instancetype)init;
- (ODClassType*)type;
- (void)appendItem:(id)item;
- (CNImHashSet*)build;
+ (ODClassType*)type;
@end


