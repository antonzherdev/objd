#import <Foundation/Foundation.h>
#import "CNCollection.h"


@interface CNEnumerator : NSObject<CNIterator>
- (id)initWithEnumerator:(NSEnumerator *)enumerator;

+ (id)enumeratorWithEnumerator:(NSEnumerator *)enumerator;

@end


@interface CNMEnumerator : NSObject<CNMIterator>
- (id)initWithEnumerator:(NSEnumerator *)enumerator;

+ (id)enumeratorWithEnumerator:(NSEnumerator *)enumerator;

@end