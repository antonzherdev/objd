#import <Foundation/Foundation.h>
#import "CNTypes.h"

@protocol CNIterable;


@interface CNZipLink : NSObject <CNChainLink>
- (id)initWithA:(id <CNIterable>)a by:(cnF2)f;

+ (id)linkWithA:(id <CNIterable>)a by:(cnF2)f;
@end

@interface CNZip3Link : NSObject <CNChainLink>
- (id)initWithA:(id <CNIterable>)a b:(id <CNIterable>)b by:(cnF3)f;

+ (id)linkWithA:(id <CNIterable>)a b:(id <CNIterable>)b by:(cnF3)f;
@end