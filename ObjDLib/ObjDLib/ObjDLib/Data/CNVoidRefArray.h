#import "objd.h"
#import "ODType.h"
@class CNPArray;
@class CNPArrayIterator;
#import "CNTypes.h"

typedef struct CNVoidRefArray CNVoidRefArray;

struct CNVoidRefArray {
    NSUInteger length;
    VoidRef bytes;
};
static inline CNVoidRefArray CNVoidRefArrayMake(NSUInteger length, VoidRef bytes) {
    return (CNVoidRefArray){length, bytes};
}
static inline BOOL CNVoidRefArrayEq(CNVoidRefArray s1, CNVoidRefArray s2) {
    return s1.length == s2.length && s1.bytes == s2.bytes;
}
static inline NSUInteger CNVoidRefArrayHash(CNVoidRefArray self) {
    return (NSUInteger) self.bytes;
}
static inline NSString* CNVoidRefArrayDescription(CNVoidRefArray self) {
    NSMutableString* description = [NSMutableString stringWithString:@"<CNVoidRefArray: "];
    [description appendFormat:@"length=%lu", (unsigned long)self.length];
    [description appendFormat:@", bytes=%p", self.bytes];
    [description appendString:@">"];
    return description;
}
CNVoidRefArray cnVoidRefArrayApplyLength(NSUInteger length);
CNVoidRefArray cnVoidRefArrayApplyTpCount(ODPType* tp, NSUInteger count);
#define cnVoidRefArrayWriteTpItem(p_self, p_tp, p_item) ({\
    *(p_tp*)p_self.bytes = p_item;\
    NSUInteger __s = sizeof(p_tp);\
    (CNVoidRefArray){p_self.length - __s, ((char*)p_self.bytes) + __s};\
})
static inline CNVoidRefArray cnVoidRefArrayWriteUInt4(const CNVoidRefArray self, unsigned int uInt4) {
    *(unsigned int*)self.bytes = uInt4;
    return (CNVoidRefArray){self.length - 4, ((unsigned int*)self.bytes) + 1};
}
static inline CNVoidRefArray cnVoidRefArrayAddBytes(const CNVoidRefArray self, NSUInteger bytes) {
    return (CNVoidRefArray){self.length - bytes, ((char*)self.bytes) + bytes};
}
static inline CNVoidRefArray cnVoidRefArraySubBytes(const CNVoidRefArray self, NSUInteger bytes) {
    return (CNVoidRefArray){self.length + bytes, ((char*)self.bytes) - bytes};
}


static inline void cnVoidRefArrayFree(CNVoidRefArray self) {
    free(self.bytes);
}

ODPType* cnVoidRefArrayType();
@interface CNVoidRefArrayWrap : NSObject
@property (readonly, nonatomic) CNVoidRefArray value;

+ (id)wrapWithValue:(CNVoidRefArray)value;
- (id)initWithValue:(CNVoidRefArray)value;
@end



