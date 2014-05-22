#import "objd.h"
#import "CNChain.h"

#import "CNType.h"
#import "CNYield.h"
#import "CNSourceLink.h"
#import "CNFilterLink.h"
#import "CNMapLink.h"
#import "CNCombinationsLink.h"
#import "CNMulLink.h"
#import "CNSeq.h"
#import "CNGroupByLink.h"
#import "CNZipLink.h"
#import "CNSortLink.h"
#import "CNSortBuilder.h"
#import "CNSeed.h"
#import "CNPlat.h"
#import "CNList.h"
#import "CNSet.h"
#import "CNTreeSet.h"
#import "CNMap.h"
#import "CNString.h"
#import "CNFuture.h"
#import "CNFutureEnd.h"
@implementation CNChain
static CNClassType* _CNChain_type;
@synthesize link = _link;
@synthesize previous = _previous;

+ (instancetype)chainWithLink:(id<CNChainLink>)link previous:(CNChain*)previous {
    return [[CNChain alloc] initWithLink:link previous:previous];
}

- (instancetype)initWithLink:(id<CNChainLink>)link previous:(CNChain*)previous {
    self = [super init];
    if(self) {
        _link = link;
        _previous = previous;
    }
    
    return self;
}

+ (void)initialize {
    [super initialize];
    if(self == [CNChain class]) _CNChain_type = [CNClassType classTypeWithCls:[CNChain class]];
}

+ (CNChain*)applyCollection:(id<CNTraversable>)collection {
    return [CNChain chainWithLink:[CNSourceLink sourceLinkWithCollection:collection] previous:nil];
}

- (CNChain*)filterFactor:(CGFloat)factor when:(BOOL(^)(id))when {
    return [self addLink:[CNFilterLink filterLinkWithFactor:factor predicate:when]];
}

- (CNChain*)filterWhen:(BOOL(^)(id))when {
    return [self filterFactor:0.5 when:when];
}

- (CNChain*)topNumbers:(NSInteger)numbers {
    return [self addLink:[CNTopLink topLinkWithNumber:((NSUInteger)(numbers))]];
}

- (CNChain*)filterCastFactor:(CGFloat)factor to:(CNClassType*)to {
    return ((CNChain*)([self addLink:[CNFilterLink filterLinkWithFactor:factor predicate:^BOOL(id item) {
        return [to isInstanceObj:item];
    }]]));
}

- (CNChain*)filterCastTo:(CNClassType*)to {
    return [self filterCastFactor:0.5 to:to];
}

- (CNChain*)mapF:(id(^)(id))f {
    return [self addLink:[CNMapLink mapLinkWithF:f]];
}

- (CNChain*)mapOptF:(id(^)(id))f {
    return [self addLink:[CNMapOptLink mapOptLinkWithF:f]];
}

- (CNChain*)flatMapFactor:(CGFloat)factor f:(id<CNTraversable>(^)(id))f {
    return [self addLink:[CNFlatMapLink flatMapLinkWithFactor:factor f:f]];
}

- (CNChain*)flatMapF:(id<CNTraversable>(^)(id))f {
    return [self flatMapFactor:2.0 f:f];
}

- (CNChain*)flatFactor:(CGFloat)factor {
    return [self addLink:[CNFlatLink flatLinkWithFactor:factor]];
}

- (CNChain*)flat {
    return [self flatFactor:2.0];
}

- (CNChain*)combinations {
    return [self addLink:[CNCombinationsLink combinationsLink]];
}

- (CNChain*)uncombinations {
    return [self addLink:[CNUncombinationsLink uncombinationsLink]];
}

- (CNChain*)neighbours {
    return [self addLink:[CNNeighboursLink neighboursLinkWithRing:NO]];
}

- (CNChain*)neighboursRing {
    return [self addLink:[CNNeighboursLink neighboursLinkWithRing:YES]];
}

- (CNChain*)mulBy:(id<CNTraversable>)by {
    return [self addLink:[CNMulLink mulLinkWithCollection:by]];
}

- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by {
    return [self addLink:[CNMGroupByLink groupByLinkWithFactor:factor by:by start:^CNArrayBuilder*() {
        return [CNArrayBuilder apply];
    } append:^void(CNArrayBuilder* b, id item) {
        [((CNArrayBuilder*)(b)) appendItem:item];
    } finish:^CNImArray*(CNArrayBuilder* b) {
        return [((CNArrayBuilder*)(b)) build];
    }]];
}

- (CNChain*)groupBy:(id(^)(id))by {
    return [self groupFactor:0.5 by:by];
}

- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by map:(id(^)(id))map {
    __weak CNChain* _weakSelf = self;
    return [self addLink:[CNMGroupByLink groupByLinkWithFactor:factor by:by start:^CNArrayBuilder*() {
        return [CNArrayBuilder apply];
    } append:^void(CNArrayBuilder* b, id item) {
        CNChain* _self = _weakSelf;
        if(_self != nil) [((CNArrayBuilder*)(b)) appendItem:[_self mapF:^id(id _) {
            return item;
        }]];
    } finish:^CNImArray*(CNArrayBuilder* b) {
        return [((CNArrayBuilder*)(b)) build];
    }]];
}

- (CNChain*)groupBy:(id(^)(id))by map:(id(^)(id))map {
    return [self groupFactor:0.5 by:by map:map];
}

- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by builder:(id<CNBuilder>(^)())builder {
    return [self addLink:[CNMGroupByLink groupByLinkWithFactor:factor by:by start:builder append:^void(id<CNBuilder> b, id item) {
        [((id<CNBuilder>)(b)) appendItem:item];
    } finish:^id(id<CNBuilder> b) {
        return [((id<CNBuilder>)(b)) build];
    }]];
}

- (CNChain*)groupBy:(id(^)(id))by builder:(id<CNBuilder>(^)())builder {
    return [self groupFactor:0.5 by:by builder:builder];
}

- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by map:(id(^)(id))map builder:(id<CNBuilder>(^)())builder {
    __weak CNChain* _weakSelf = self;
    return [self addLink:[CNMGroupByLink groupByLinkWithFactor:factor by:by start:builder append:^void(id<CNBuilder> b, id item) {
        CNChain* _self = _weakSelf;
        if(_self != nil) [((id<CNBuilder>)(b)) appendItem:[_self mapF:^id(id _) {
            return item;
        }]];
    } finish:^id(id<CNBuilder> b) {
        return [((id<CNBuilder>)(b)) build];
    }]];
}

- (CNChain*)groupBy:(id(^)(id))by map:(id(^)(id))map builder:(id<CNBuilder>(^)())builder {
    return [self groupFactor:0.5 by:by map:map builder:builder];
}

- (CNChain*)groupFactor:(CGFloat)factor by:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold {
    return [self addLink:[CNImGroupByLink imGroupByLinkWithFactor:factor by:by start:start fold:fold]];
}

- (CNChain*)groupBy:(id(^)(id))by start:(id(^)())start fold:(id(^)(id, id))fold {
    return [self groupFactor:0.5 by:by start:start fold:fold];
}

- (CNChain*)distinctFactor:(CGFloat)factor {
    return [self addLink:[CNDistinctLink distinctLinkWithFactor:factor]];
}

- (CNChain*)distinct {
    return [self distinctFactor:0.5];
}

- (CNChain*)zipB:(id<CNIterable>)b {
    return [self addLink:[CNZipLink zipLinkWithA:b f:^CNTuple*(id aa, id bb) {
        return tuple(aa, bb);
    }]];
}

- (CNChain*)zipB:(id<CNIterable>)b by:(id(^)(id, id))by {
    return [self addLink:[CNZipLink zipLinkWithA:b f:by]];
}

- (void)zipForB:(id<CNIterable>)b by:(void(^)(id, id))by {
    id<CNIterator> bi = [b iterator];
    [self applyYield:[CNYield makeYield:^CNGoR(id a) {
        if([bi hasNext]) {
            by(a, [bi next]);
            return CNGo_Continue;
        } else {
            return CNGo_Break;
        }
    }]];
}

- (CNChain*)zip3B:(id<CNIterable>)b c:(id<CNIterable>)c {
    return [self addLink:[CNZip3Link zip3LinkWithA:b b:c f:^CNTuple3*(id aa, id bb, id cc) {
        return tuple3(aa, bb, cc);
    }]];
}

- (CNChain*)zip3B:(id<CNIterable>)b c:(id<CNIterable>)c by:(id(^)(id, id, id))by {
    return [self addLink:[CNZip3Link zip3LinkWithA:b b:c f:by]];
}

- (CNChain*)prependCollection:(id<CNTraversable>)collection {
    return [self addLink:[CNPrependLink prependLinkWithCollection:collection]];
}

- (CNChain*)appendCollection:(id<CNTraversable>)collection {
    return [self addLink:[CNAppendLink appendLinkWithCollection:collection]];
}

- (CNChain*)excludeCollection:(id<CNTraversable>)collection {
    id<CNTraversable> c = (([collection isKindOfClass:[CNChain class]]) ? ((id<CNTraversable>)([((CNChain*)(collection)) toSet])) : collection);
    return [self filterWhen:^BOOL(id item) {
        return !([c containsItem:item]);
    }];
}

- (CNChain*)intersectCollection:(id<CNIterable>)collection {
    id<CNTraversable> c = (([collection isKindOfClass:[CNChain class]]) ? ((id<CNTraversable>)([((CNChain*)(collection)) toSet])) : collection);
    return [self filterWhen:^BOOL(id item) {
        return [c containsItem:item];
    }];
}

- (CNChain*)reverse {
    return [self addLink:[CNReverseLink reverseLink]];
}

- (CNChain*)reverseWhen:(BOOL)when {
    if(when) return ((CNChain*)([self addLink:[CNReverseLink reverseLink]]));
    else return self;
}

- (CNChain*)sort {
    return [self addLink:[CNSortLink sortLinkWithComparator:^NSInteger(id a, id b) {
        return [a compareTo:b];
    }]];
}

- (CNChain*)sortDesc {
    return [self addLink:[CNSortLink sortLinkWithComparator:^NSInteger(id a, id b) {
        return -[a compareTo:b];
    }]];
}

- (CNChain*)sortComparator:(NSInteger(^)(id, id))comparator {
    return [self addLink:[CNSortLink sortLinkWithComparator:comparator]];
}

- (CNSortBuilder*)sortBy {
    return [CNSortBuilder sortBuilderWithChain:self];
}

- (CNChain*)shuffle {
    return [self addLink:[CNShuffleLink shuffleLink]];
}

- (CNGoR)goOn:(CNGoR(^)(id))on {
    return [self applyYield:[CNYield makeYield:on]];
}

- (id)foldStart:(id)start by:(id(^)(id, id))by {
    __block id r = start;
    [self applyYield:[CNYield makeYield:^CNGoR(id item) {
        r = by(r, item);
        return CNGo_Continue;
    }]];
    return r;
}

- (NSUInteger)count {
    __block NSInteger r = 0;
    [self _forEach:^void(id _) {
        r++;
    }];
    return ((NSUInteger)(r));
}

- (void)_forEach:(void(^)(id))each {
    [self applyYield:[CNYield makeYield:^CNGoR(id item) {
        each(item);
        return CNGo_Continue;
    }]];
}

- (id)last {
    __block id ret;
    [self _forEach:^void(id item) {
        ret = item;
    }];
    return ret;
}

- (id)randomItem {
    id<CNSeq> s = [self toSeq];
    NSUInteger n = [s count];
    if(n == 0) return nil;
    else return [s applyIndex:cnuIntRndMax(n - 1)];
}

- (id)randomItemSeed:(CNSeed*)seed {
    id<CNSeq> s = [self toSeq];
    NSUInteger n = [s count];
    if(n == 0) return nil;
    else return [s applyIndex:((NSUInteger)([seed nextIntMin:0 max:((int)(n - 1))]))];
}

- (BOOL)isEmpty {
    __block BOOL ret = YES;
    [self applyYield:[CNYield makeYield:^CNGoR(id item) {
        ret = YES;
        return CNGo_Break;
    }]];
    return ret;
}

- (CNTuple*)gap {
    __block id min;
    __block id max;
    [self _forEach:^void(id item) {
        if(min == nil || [min compareTo:item] > 0) min = item;
        if(max == nil || [max compareTo:item] < 0) max = item;
    }];
    if(min == nil) return nil;
    else return tuple(min, ((id)(max)));
}

- (id)min {
    __block id min;
    [self _forEach:^void(id item) {
        if(min == nil || [min compareTo:item] > 0) min = item;
    }];
    return min;
}

- (id)max {
    __block id max;
    [self _forEach:^void(id item) {
        if(max == nil || [max compareTo:item] < 0) max = item;
    }];
    return max;
}

- (BOOL)or {
    __block BOOL ret = NO;
    [self applyYield:[CNYield makeYield:^CNGoR(id item) {
        if(unumb(item)) {
            ret = YES;
            return CNGo_Break;
        } else {
            return CNGo_Continue;
        }
    }]];
    return ret;
}

- (BOOL)and {
    __block BOOL ret = YES;
    [self applyYield:[CNYield makeYield:^CNGoR(id item) {
        if(!(unumb(item))) {
            ret = NO;
            return CNGo_Break;
        } else {
            return CNGo_Continue;
        }
    }]];
    return ret;
}

- (id)convertTo:(CNType*)to builder:(id<CNBuilder>(^)(NSInteger))builder {
    __block id<CNBuilder> b;
    __block id r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        b = builder(((NSInteger)(size)));
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((id<CNBuilder>)(nonnil(b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if(NO) {
            r = ((id)(all));
            return CNGo_Continue;
        } else {
            return [yield stdYieldAllItems:all];
        }
    }]];
    if(r == nil) return [((id<CNBuilder>)(nonnil(b))) build];
    else return r;
}

- (id<CNSeq>)toSeq {
    __block id<CNBuilder> __il_b;
    __block id<CNSeq> __il_r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        __il_b = ({
            NSInteger _ = ((NSInteger)(size));
            [CNArrayBuilder arrayBuilderWithCapacity:((NSUInteger)(_))];
        });
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((id<CNBuilder>)(nonnil(__il_b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if([all conformsToProtocol:@protocol(CNSeq)]) {
            __il_r = ((id)(all));
            return CNGo_Continue;
        } else {
            return [yield stdYieldAllItems:all];
        }
    }]];
    if(__il_r == nil) return [((id<CNBuilder>)(nonnil(__il_b))) build];
    else return __il_r;
}

- (NSArray*)toArray {
    __block CNArrayBuilder* b;
    __block NSArray* r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        b = [CNArrayBuilder arrayBuilderWithCapacity:size];
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((CNArrayBuilder*)(nonnil(b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if([all isKindOfClass:[CNImArray class]]) {
            r = ((CNImArray*)(all));
            return CNGo_Continue;
        } else {
            if([all isKindOfClass:[CNMArray class]]) {
                r = ((CNMArray*)(all));
                return CNGo_Continue;
            } else {
                return [yield stdYieldAllItems:all];
            }
        }
    }]];
    if(r == nil) return [((CNArrayBuilder*)(nonnil(b))) build];
    else return r;
}

- (CNImList*)toList {
    __block id<CNBuilder> __il_b;
    __block CNImList* __il_r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        __il_b = ({
            NSInteger _ = ((NSInteger)(size));
            [CNImListBuilder imListBuilder];
        });
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((id<CNBuilder>)(nonnil(__il_b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if([all isKindOfClass:[CNImList class]]) {
            __il_r = ((id)(all));
            return CNGo_Continue;
        } else {
            return [yield stdYieldAllItems:all];
        }
    }]];
    if(__il_r == nil) return [((id<CNBuilder>)(nonnil(__il_b))) build];
    else return __il_r;
}

- (id<CNSet>)toSet {
    __block id<CNBuilder> __il_b;
    __block id<CNSet> __il_r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        __il_b = ({
            NSInteger _ = ((NSInteger)(size));
            [CNHashSetBuilder hashSetBuilderWithCapacity:((NSUInteger)(_))];
        });
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((id<CNBuilder>)(nonnil(__il_b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if([all conformsToProtocol:@protocol(CNSet)]) {
            __il_r = ((id)(all));
            return CNGo_Continue;
        } else {
            return [yield stdYieldAllItems:all];
        }
    }]];
    if(__il_r == nil) return [((id<CNBuilder>)(nonnil(__il_b))) build];
    else return __il_r;
}

- (CNTreeSet*)toTreeSet {
    __block id<CNBuilder> __il_b;
    __block CNTreeSet* __il_r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        __il_b = ({
            NSInteger _ = ((NSInteger)(size));
            [CNTreeSetBuilder apply];
        });
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((id<CNBuilder>)(nonnil(__il_b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if([all isKindOfClass:[CNTreeSet class]]) {
            __il_r = ((id)(all));
            return CNGo_Continue;
        } else {
            return [yield stdYieldAllItems:all];
        }
    }]];
    if(__il_r == nil) return [((id<CNBuilder>)(nonnil(__il_b))) build];
    else return __il_r;
}

- (NSDictionary*)toMap {
    __block id<CNBuilder> __il_b;
    __block CNImHashMap* __il_r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        __il_b = ({
            NSInteger _ = ((NSInteger)(size));
            [CNHashMapBuilder hashMapBuilder];
        });
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((id<CNBuilder>)(nonnil(__il_b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if([all isKindOfClass:[CNImHashMap class]]) {
            __il_r = ((id)(all));
            return CNGo_Continue;
        } else {
            return [yield stdYieldAllItems:all];
        }
    }]];
    if(__il_r == nil) return [((id<CNBuilder>)(nonnil(__il_b))) build];
    else return __il_r;
}

- (NSString*)toStringStart:(NSString*)start delimiter:(NSString*)delimiter end:(NSString*)end {
    CNStringBuilder* b = [CNStringBuilder stringBuilder];
    [b appendStr:start];
    __block BOOL first = YES;
    [self _forEach:^void(id item) {
        if(first) first = NO;
        else [b appendStr:delimiter];
        [b appendObj:item];
    }];
    [b appendStr:end];
    return [b build];
}

- (NSString*)toStringDelimiter:(NSString*)delimiter {
    return [self toStringStart:@"" delimiter:delimiter end:@""];
}

- (NSString*)toString {
    __block id<CNBuilder> __il_b;
    __block CNString* __il_r;
    [self applyYield:[CNYield makeBegin:^CNGoR(NSUInteger size) {
        __il_b = ({
            NSInteger _ = ((NSInteger)(size));
            [CNStringBuilder stringBuilder];
        });
        return CNGo_Continue;
    } yield:^CNGoR(id item) {
        [((id<CNBuilder>)(nonnil(__il_b))) appendItem:item];
        return CNGo_Continue;
    } all:^CNGoR(CNYield* yield, id<CNTraversable> all) {
        if([all isKindOfClass:[CNString class]]) {
            __il_r = ((id)(all));
            return CNGo_Continue;
        } else {
            return [yield stdYieldAllItems:all];
        }
    }]];
    if(__il_r == nil) return [((id<CNBuilder>)(nonnil(__il_b))) build];
    else return __il_r;
}

- (CNFuture*)futureF:(id(^)(CNChain*))f {
    CNFutureEnd* lnk = [CNFutureEnd futureEnd];
    [self applyYield:[lnk yield]];
    return [[lnk future] mapF:^id(NSArray* o) {
        return f([((NSArray*)(o)) chain]);
    }];
}

- (CNFuture*)future {
    CNFutureEnd* lnk = [CNFutureEnd futureEnd];
    [self applyYield:[lnk yield]];
    return [lnk future];
}

- (CNFuture*)voidFuture {
    CNFutureVoidEnd* lnk = [CNFutureVoidEnd futureVoidEnd];
    [self applyYield:[lnk yield]];
    return [lnk future];
}

- (CNGoR)applyYield:(CNYield*)yield {
    CNYield* y = [self buildYield:yield];
    CNGoR r = [y beginYieldWithSize:0];
    return [y endYieldWithResult:r];
}

- (CNYield*)buildYield:(CNYield*)yield {
    CNChain* ch = self;
    CNYield* y = yield;
    while(ch != nil) {
        y = [((CNChain*)(ch)).link buildYield:y];
        ch = ((CNChain*)(ch)).previous;
    }
    return y;
}

- (CNChain*)addLink:(id<CNChainLink>)link {
    return [CNChain chainWithLink:link previous:self];
}

+ (id<CNTraversable>)resolveCollection:(id<CNTraversable>)collection {
    if([collection isKindOfClass:[CNChain class]]) return ((id<CNTraversable>)([((CNChain*)(collection)) toArray]));
    else return collection;
}

+ (id<CNTraversable>)resolveToSetCollection:(id<CNTraversable>)collection {
    if([collection isKindOfClass:[CNChain class]]) return ((id<CNTraversable>)([((CNChain*)(collection)) toSet]));
    else return collection;
}

- (CNClassType*)type {
    return [CNChain type];
}

+ (CNClassType*)type {
    return _CNChain_type;
}

- (id)copyWithZone:(NSZone*)zone {
    return self;
}

- (NSString*)description {
    NSMutableString* description = [NSMutableString stringWithFormat:@"<%@: ", NSStringFromClass([self class])];
    [description appendFormat:@"link=%@", self.link];
    [description appendFormat:@", previous=%@", self.previous];
    [description appendString:@">"];
    return description;
}

@end

