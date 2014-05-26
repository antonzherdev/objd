#import "objdcore.h"
#import "CNCollection.h"
@class CNClassType;
@class CNDispatchQueue;
@class CNObject;
@class CNChain;
@class CNMHashSet;
@class CNImHashSet;

@class CNSet_impl;
@class CNImSet_impl;
@class CNMSet_impl;
@class CNHashSetBuilder;
@protocol CNSet;
@protocol CNImSet;
@protocol CNMSet;

@protocol CNSet<CNIterable>
- (NSString*)description;
@end


@interface CNSet_impl : CNIterable_impl<CNSet>
@end


@protocol CNImSet<CNSet, CNImIterable>
- (id<CNMSet>)mCopy;
- (NSString*)description;
@end


@interface CNImSet_impl : CNSet_impl<CNImSet>
- (id<CNMSet>)mCopy;
@end


@protocol CNMSet<CNSet, CNMIterable>
- (id<CNImSet>)im;
- (id<CNImSet>)imCopy;
- (NSString*)description;
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

+ (instancetype)hashSetBuilderWithCapacity:(NSUInteger)capacity;
- (instancetype)initWithCapacity:(NSUInteger)capacity;
- (CNClassType*)type;
- (void)appendItem:(id)item;
- (CNImHashSet*)build;
+ (CNHashSetBuilder*)apply;
- (NSString*)description;
+ (CNClassType*)type;
@end


