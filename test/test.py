import requests
from urllib.parse import urlencode
import json

host = 'http://127.0.0.1:3000'

def api(uri, query = None):
    if query:
        return "{}/api/{}/?{}".format(host, uri, urlencode(query))

    return "{}/api/{}".format(host, uri)

def preprocess(rsp):
    print(rsp.text)
    data = rsp.json()
    if data.get('err'):
        raise Exception(data['err'])
    return data

def check_equal(a, b, key = None):
    if key is None:
        if a != b:
            raise Exception("Value must Equal. except: {} but got {}".format(a, b))
    else:
        if a[key] != b[key]:
            raise Exception("Value must Equal. except: {} but got {}".format(a[key], b[key]))

def run_post(uri, data = None, query=None):
    rsp = requests.post(api(uri, query), data = data)
    return preprocess(rsp)

def run_get(uri, query = None):
    rsp = requests.get(api(uri, query))
    return preprocess(rsp)

def run_delete(uri, query = None):
    rsp = requests.delete(api(uri, query))
    return preprocess(rsp)

def create_user(username, password):
    return run_post('users', {'username': username, 'passwd': password})

def get_user(uidOrName):
    return run_get("users/{}".format(uidOrName))

def delete_user(uidOrName):
    return run_delete("users/{}".format(uidOrName))

def get_users(from_ = 0, size = 100):
    return run_get("users", query={"from": from_, "size": size})

def update_user_name(uidOrName, new_name):
    return run_post("users/{}".format(uidOrName), data = { 'username': new_name })

def verify_password(uidOrName, password):
    return run_post("users/{}/verify".format(uidOrName), data={"passwd": password})

def update_password(uidOrName, password):
    return run_post("users/{}/passwd".format(uidOrName), data={"passwd": password})

def update_extra(uidOrName, extra):
    return run_post("users/{}/extra".format(uidOrName), data={"extra": json.dumps(extra)})

def update_secure_extra(uidOrName, extra):
    return run_post("users/{}/secure_extra".format(uidOrName), data={"extra": json.dumps(extra)})

# def remove_extra(uidOrName, extra):
#     return run_delete("users/{}/extra".format(uidOrName), data={"extra": json.dumps(extra)})
#
# def remove_secure_extra(uidOrName, extra):
#     return run_delete("users/{}/secure_extra".format(uidOrName), data={"extra": json.dumps(extra)})

def clear_extra(uidOrName):
    return run_post("users/{}/extra/clear".format(uidOrName))

def clear_secure_extra(uidOrName):
    return run_post("users/{}/secure_extra/clear".format(uidOrName))

def create_bind(uidOrName, service, name, extra = {}):
    return run_post("users/{}/binds".format(uidOrName), data = {
        "service": service,
        "name": name,
        "extra": json.dumps(extra)
    })


def get_binds(uidOrName, from_ = 0, size = 100):
    return run_get("users/{}/binds".format(uidOrName), {'from': from_, 'size': size})

def get_bind(name):
    return run_get("binds", {'name': name})

def update_bind_extra(bidOrName, extra = {}):
    return run_post("binds/{}".format(bidOrName), {'extra': json.dumps(extra)})

def remove_bind(bidOrName):
    return run_delete("binds/{}".format(bidOrName))

def get_binds_by_service(uidOrName, service, from_ = 0, size = 100):
    return run_get("users/{}/binds/{}".format(uidOrName, service), { 'from': from_, 'size': size })

def get_all_binds_by_service(service, from_ = 0, size = 100):
    return run_get("service/{}/binds".format(service), { 'from': from_, 'size': size })

def save_group_meta(group, title, summary = ""):
    return run_post("groupmeta/{}".format(group), {'title': title, 'summary': summary})

def get_group_meta(group):
    return run_get("groupmeta/{}".format(group))

def remove_group_meta(group):
    return run_delete("groupmeta/{}".format(group))

def create_group(group, uidOrName):
    return run_post('groups/{}/{}'.format(group, uidOrName))

def remove_group(group, uidOrName):
    return run_delete('groups/{}/{}'.format(group, uidOrName))

def get_group_users(group, from_ = 0, size = 100):
    return run_get('groups/{}'.format(group), { 'from': from_, 'size': size })

def check_users(total):
    from_ = 0
    size = 100
    ret = get_users(from_, size)
    check_equal(ret['total'], total)
    check_equal(ret['size'], size)
    check_equal(ret['from'], from_)

def clean_users():
    ret = get_users()
    for user in ret['users']:
        ret = delete_user(user['name'])
        check_equal(ret, {'result': 'OK'}, 'result')

def test_bind(username):
    # create bind
    bind_name = "phone-123456789"

    bind = create_bind(username, 'phone', bind_name, {"test": "test"})
    check_equal(bind['name'], bind_name)
    check_equal(bind['service'], 'phone')
    check_equal(bind['extra'], {"test": "test"}, "test")

    user = get_user(username)
    check_equal(user['id'], bind['user_id'])
    check_equal(user['binds'][0], bind, "id")
    check_equal(user['binds'][0], bind, "service")
    check_equal(user['binds'][0], bind, "name")

    bind1 = get_bind(bind_name)
    check_equal(bind1, bind, "id")
    check_equal(bind1, bind, "service")
    check_equal(bind1, bind, "name")

    binds = get_binds(username)
    check_equal(binds['binds'][0], bind, "id")
    check_equal(binds['binds'][0], bind, "service")
    check_equal(binds['binds'][0], bind, "name")

    for bname in ['a', 'b', 'c']:
        bind1 = create_bind(username, bname, bname, {bname: bname})
        check_equal(bind1['name'], bname)
        check_equal(bind1['service'], bname)
        check_equal(bind1['extra'], {bname: bname}, bname)

    binds = get_binds_by_service(username, 'a')
    check_equal(binds['binds'][0]['name'], 'a')
    check_equal(binds['binds'][0]['service'], 'a')
    check_equal(binds['binds'][0]['extra'], {'a': 'a'}, 'a')

    binds = get_binds_by_service(username, 'b')
    check_equal(binds['binds'][0]['name'], 'b')
    check_equal(binds['binds'][0]['service'], 'b')
    check_equal(binds['binds'][0]['extra'], {'b': 'b'}, 'b')

    binds = get_binds_by_service(username, 'c')
    check_equal(binds['binds'][0]['name'], 'c')
    check_equal(binds['binds'][0]['service'], 'c')
    check_equal(binds['binds'][0]['extra'], {'c': 'c'}, 'c')

    binds = get_all_binds_by_service('a')
    check_equal(binds['binds'][0]['name'], 'a')
    check_equal(binds['binds'][0]['service'], 'a')
    check_equal(binds['binds'][0]['extra'], {'a': 'a'}, 'a')

    binds = get_all_binds_by_service('b')
    check_equal(binds['binds'][0]['name'], 'b')
    check_equal(binds['binds'][0]['service'], 'b')
    check_equal(binds['binds'][0]['extra'], {'b': 'b'}, 'b')

    binds = get_all_binds_by_service('c')
    check_equal(binds['binds'][0]['name'], 'c')
    check_equal(binds['binds'][0]['service'], 'c')
    check_equal(binds['binds'][0]['extra'], {'c': 'c'}, 'c')

    ret = update_bind_extra('a', {'a': 'new_a'})
    check_equal(ret, {'result': 'OK'}, 'result')
    bind1 = get_bind('a')
    check_equal(bind1['extra'], {'a': 'new_a'}, "a")

    ret = remove_bind('a')
    check_equal(ret, {'result': 'OK'}, 'result')
    binds = get_all_binds_by_service('a')
    check_equal(len(binds['binds']), 0)


def test_group_meta(username):
    group = 'admin'
    title = 'Admin'
    summary = 'summary'

    ret = save_group_meta(group, title, summary)
    check_equal(ret, {'result': 'OK'}, 'result')

    meta = get_group_meta(group)
    check_equal(meta['summary'], summary)
    check_equal(meta['title'], title)
    check_equal(meta['group'], group)
    check_equal(meta['user_count'], 0)

    ret = create_group(group, username)
    check_equal(ret, {'result': 'OK'}, 'result')

    meta = get_group_meta(group)
    check_equal(meta['user_count'], 1)

    users = get_group_users(group)
    check_equal(users['users'][0]['name'], username)
    check_equal(len(users['users']), 1)

    ret = remove_group(group, username)
    check_equal(ret, {'result': 'OK'}, 'result')

    users = get_group_users(group)
    check_equal(len(users['users']), 0)

    meta = get_group_meta(group)
    check_equal(meta['user_count'], 0)

    ret = remove_group_meta(group)
    check_equal(ret, {'result': 'OK'}, 'result')

    try:
        get_group_meta(group)
    except Exception as e:
        check_equal(str(e), 'GroupMeta not found.')

def main():
    username = 'Lupino'
    username1 = "Lupino1"
    password = 'Lupino'
    password1 = 'Lupino1'

    clean_users()

    check_users(0)
    user1 = create_user(username, password)
    check_users(1)

    user2 = get_user(user1['name'])
    check_equal(user1, user2, 'id')
    check_equal(user1, user2, 'name')
    check_equal(username, user1['name'])
    check_equal(username, user2['name'])

    ret = verify_password(username, password)
    check_equal(ret, {'result': 'OK'}, 'result')

    ret = update_password(username, password1)
    check_equal(ret, {'result': 'OK'}, 'result')

    ret = verify_password(username, password1)
    check_equal(ret, {'result': 'OK'}, 'result')

    # test update extra
    ret = update_extra(username, {'test1': "test"})
    check_equal(ret, {'result': 'OK'}, 'result')
    user4 = get_user(username)
    check_equal(user4['extra']['test1'], "test")

    ret = update_extra(username, {'test1': "test1"})
    check_equal(ret, {'result': 'OK'}, 'result')
    user4 = get_user(username)
    check_equal(user4['extra']['test1'], "test1")

    # test secure_extra
    ret = update_secure_extra(username, {'test1': "test_secure"})
    check_equal(ret, {'result': 'OK'}, 'result')
    user4 = get_user(username)
    check_equal(user4['extra']['test1'], "test_secure")

    ret = update_extra(username, {'test1': "test1"})
    check_equal(ret, {'result': 'OK'}, 'result')
    user4 = get_user(username)
    check_equal(user4['extra']['test1'], "test_secure")

    # # test remove extra

    # ret = remove_secure_extra(username, {"test1": ""})
    # check_equal(ret, {'result': 'OK'}, 'result')
    # user4 = get_user(username)
    # check_equal(user4['extra']['test1'], "test1")

    # ret = remove_extra(username, {"test1": ""})
    # check_equal(ret, {'result': 'OK'}, 'result')
    # user4 = get_user(username)
    # check_equal(user4['extra'].get('test1'), None)

    # test clear extra

    ret = clear_secure_extra(username)
    check_equal(ret, {'result': 'OK'}, 'result')
    user4 = get_user(username)
    check_equal(user4['extra']['test1'], "test1")

    ret = clear_extra(username)
    check_equal(ret, {'result': 'OK'}, 'result')
    user4 = get_user(username)
    check_equal(user4['extra'], None)

    # update user name
    ret = update_user_name(username, username1)
    check_equal(ret, {'result': 'OK'}, 'result')

    user3 = get_user(user1['id'])
    check_equal(user1, user3, 'id')
    check_equal(username1, user3['name'])

    test_bind(username1)
    test_group_meta(username1)

    clean_users()

    check_users(0)

main()
