#import "objdcore.h"
#import "CNCollection.h"
@class CNDispatchQueue;
@class CNChain;
@class CNMHashSet;
@class CNImHashSet;
@class ODClassType;

@class CNSet_impl;
@class CNImSet_impl;
@class CNMSet_impl;
@class CNHashSetBuilder;
@protocol CNSet;
@protocol CNImSet;
@protocol CNMSet;

@protocol CNSet<CNIterable>
@end


@interface CNSet_impl : CNIterable_impl<CNSet>
@end


@protocol CNImSet<CNSet, CNImIterable>
- (id<CNMSet>)mCopy;
@end


@interface CNImSet_impl : CNSet_impl<CNImSet>
- (id<CNMSet>)mCopy;
@end


@protocol CNMSet<CNSet, CNMIterable>
- (id<CNImSet>)im;
- (id<CNImSet>)imCopy;
@end


@interface CNMSet_impl : CNSet_impl<CNMSet>
- (id<CNImSet>)im;
- (id<CNImSet>)imCopy;
@end


@interface CNHashSetBuilder : CNBuilder_impl {
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


