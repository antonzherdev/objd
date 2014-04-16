#import <Foundation/Foundation.h>
#import "CNTypes.h"


@interface CNSourceLink : NSObject <CNChainLink>
@property(nonatomic, strong) id collection;

- (id)initWithCollection:(id)collection;

+ (id)linkWithCollection:(id)collection;

@end